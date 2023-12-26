package zio.intellij.intentions.suggestions

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.psi.{PsiClass, PsiElement, PsiFile, PsiNamedElement, PsiPackage}
import org.jetbrains.plugins.scala.codeInsight.intention.types.AbstractTypeAnnotationIntention.complete
import org.jetbrains.plugins.scala.codeInsight.intention.types.{startTemplate, ChooseTypeTextExpression}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSimpleTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation.TypePresentation.PresentationOptions
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation.{NameRenderer, ScTypeText, TypePresentation}
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, UndefinedType, ValueType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ScType, ScalaTypeVisitor, TypePresentationContext}
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaNamesUtil
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.settings.ScalaApplicationSettings
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker.checkIntention
import zio.intellij.intentions.ZTypeAnnotationIntention
import zio.intellij.utils.TypeCheckUtils.{fromZioLayer, fromZioLike}

// borrowed from MakeTypeMoreSpecificIntention

final class SuggestTypeAlias extends ZTypeAnnotationIntention {

  override def getFamilyName: String = "Choose another type alias"

  override protected def invoke(te: ScTypeElement, declaredType: ScType, editor: Editor): Boolean = {
    implicit val tpc: TypePresentationContext = TypePresentationContext(te)
    val aliases                               = SuggestTypeAlias.findMatchingAliases(te, declaredType)
    val texts                                 = aliases.sortBy(_.presentableText.length).map(ScTypeText(_))
    val expr                                  = new ChooseTypeTextExpression(texts, ScTypeText(declaredType))
    startTemplate(te, te.getParent, expr, editor)
    true
  }

  override protected def shouldSuggest(te: ScTypeElement, declaredType: ScType): Boolean =
    SuggestTypeAlias.findMatchingAliases(te, declaredType).length > 1

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    adjustElementAtOffset(element, editor) match {
      case element if checkIntention(this, element) =>
        element.parentOfType[ScSimpleTypeElement] match {
          case Some(Typeable(tpe)) if fromZioLike(tpe) || fromZioLayer(tpe) => complete(element, descriptionStrategy)
          case _                                                            => false
        }
      case _ => false
    }

  override def generatePreview(project: Project, editor: Editor, file: PsiFile): IntentionPreviewInfo =
    IntentionPreviewInfo.EMPTY

  private def adjustElementAtOffset(element: PsiElement, editor: Editor): PsiElement =
    ScalaPsiUtil.adjustElementAtOffset(element, editor.getCaretModel.getOffset)
}

object SuggestTypeAlias {

  def allAliasesFor(tpe: ScType, context: ProjectContext, scope: GlobalSearchScope): List[ScTypeAlias] = {
    val qualifier = tpe.extractClass.flatMap {
      case t: ScTypeDefinition => Some(t.getPath)
      case _                   => None
    }

    qualifier.toList.flatMap { fqn =>
      val manager = ScalaPsiManager.instance(context)
      manager.getCachedClass(scope, fqn) match {
        case Some(z: ScObject) => z.aliases.toList
        case _                 => Nil
      }
    }
  }

  def findMatchingAliases(te: ScTypeElement, declaredType: ScType): List[ScType] = {
    val matchingAliases =
      allAliasesFor(declaredType, te.projectContext, te.resolveScope).collect {
        case alias: ScTypeAliasDefinition => conforms(alias, declaredType)
      }.flatten

    (topLevelType(declaredType) +: matchingAliases).distinct
  }

  private def topLevelType(tpe: ScType): ScType =
    tpe.aliasType match {
      case Some(AliasType(_, _, Right(value))) => value
      case _                                   => tpe
    }

  def equiv(alias: ScTypeAliasDefinition, tpe: ScType): Option[ScType] =
    check(alias, tpe)(_.equiv(tpe))

  def conforms(alias: ScTypeAliasDefinition, tpe: ScType): Option[ScType] =
    check(alias, tpe)(tpe.conforms(_))

  private def check(alias: ScTypeAliasDefinition, tpe: ScType)(checker: ScType => Boolean) = {
    val undefParams = alias.typeParameters.map(UndefinedType(_))
    val undefSubst  = ScSubstitutor.bind(alias.typeParameters, undefParams)
    alias.aliasedType.toOption.flatMap { aliasType =>
      undefSubst(aliasType)
        .conformanceSubstitutor(tpe)
        .map(subst => subst.apply(ParameterizedType(ScDesignatorType(alias), undefParams)))
        .collect { case t: ScType if checker(t) => canonicalTextHack(t) }
    }
  }

  private def canonicalTextHack(tpe: ScType): ScType = new ScType {
    private val isPreciseText = ScalaApplicationSettings.getInstance().PRECISE_TEXT

    override def isValue: Boolean = tpe.isValue

    override def inferValueType: ValueType = tpe.inferValueType

    override def visitType(visitor: ScalaTypeVisitor): Unit = tpe.visitType(visitor)

    override implicit def projectContext: ProjectContext = tpe.projectContext

    // TODO temp hack: restores the old behavior of canonicalText from before this change:
    // https://github.com/JetBrains/intellij-scala/commit/0669994c02f3eb15d9c67f09f9c696227c83060f
    // which caused some type aliases to be displayed as e.g `zioIO` instead of `zio.IO`
    override def canonicalText(context: TypePresentationContext): String = {
      val renderer: NameRenderer = new NameRenderer {
        override def renderName(e: PsiNamedElement): String          = nameFun(e, withPoint = false)
        override def renderNameWithPoint(e: PsiNamedElement): String = nameFun(e, withPoint = true)

        private def nameFun(e: PsiNamedElement, withPoint: Boolean): String = {
          val str = e match {
            case c: PsiClass =>
              val qname = c.qualifiedName
              if (qname == null || (!isPreciseText && qname == c.name)) c.name // SCL-21184
              else "_root_." + qname
            case p: PsiPackage =>
              "_root_." + p.getQualifiedName
            case _ =>
              e.nameContext match {
                case m: ScMember =>
                  m.containingClass match {
                    case o: ScObject =>
                      (if (isPreciseText && o.isStatic) "_root_." else "") + nameFun(
                        o,
                        withPoint = true
                      ) + e.name // SCL-21182
                    case _ => e.name
                  }
                case _ => e.name
              }
          }
          removeKeywords(str) + pointStr(withPoint)
        }
      }

      typeSystem.typeText(tpe, renderer, PresentationOptions(renderStdTypes = isPreciseText, canonicalForm = true))(
        context
      )
    }

    override def presentableText(implicit context: TypePresentationContext): String = tpe.presentableText

    private def removeKeywords(text: String): String =
      ScalaNamesUtil.escapeKeywordsFqn(text)

    private def pointStr(withPoint: Boolean): String =
      if (withPoint) "." else ""
  }
}
