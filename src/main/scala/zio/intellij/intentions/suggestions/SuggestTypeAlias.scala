package zio.intellij.intentions.suggestions

import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.codeInsight.intention.types.AbstractTypeAnnotationIntention.complete
import org.jetbrains.plugins.scala.codeInsight.intention.types.{startTemplate, ChooseTypeTextExpression}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScSimpleTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.presentation.ScTypeText
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, UndefinedType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.project.ProjectContext
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker.checkIntention
import zio.intellij.intentions.ZTypeAnnotationIntention
import zio.intellij.utils.TypeCheckUtils.fromZioLike

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
      case element: PsiElement if checkIntention(this, element) =>
        element.parentOfType[ScSimpleTypeElement] match {
          case Some(tpe) if tpe.`type`().toOption.exists(fromZioLike) =>
            complete(element, descriptionStrategy)
          case _ => false
        }
      case _ => false
    }

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

  def findMatchingAliases(te: ScTypeElement, declaredType: ScType): List[ScType] =
    (allAliasesFor(declaredType, te.projectContext, te.resolveScope).collect {
      case alias: ScTypeAliasDefinition =>
        val undefParams = alias.typeParameters.map(UndefinedType(_))
        val undefSubst  = ScSubstitutor.bind(alias.typeParameters, undefParams)
        alias.aliasedType.toOption
          .flatMap(aliasType =>
            undefSubst(aliasType)
              .conformanceSubstitutor(declaredType)
              .map(subst => subst.apply(ParameterizedType(ScDesignatorType(alias), undefParams)))
              .collect {
                case tpe: ScType if declaredType.conforms(tpe) => tpe
              }
          )
      // fixme distinct doesn't work
      // should compare by `equiv` instead of `==`
    }.flatten :+ topLevelType(declaredType)).distinct

  def topLevelType(tpe: ScType): ScType =
    tpe.aliasType match {
      case Some(AliasType(_, _, Right(value))) => value
      case _                                   => tpe
    }

}
