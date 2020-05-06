package zio.intellij.intentions.suggestions

import com.intellij.openapi.editor.Editor
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.codeInsight.intention.types.{ChooseTypeTextExpression, startTemplate}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.TypeAdjuster
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, ScTypeText, UndefinedType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.project.ProjectContext
import zio.intellij.intentions.ZTypeAnnotationIntention

// borrowed from MakeTypeMoreSpecificIntention

final class SuggestTypeAlias extends ZTypeAnnotationIntention {

  override def getFamilyName: String = "Choose a more specific type alias"

  override protected def invoke(te: ScTypeElement, declaredType: ScType, editor: Editor): Boolean = {
    val aliases = SuggestTypeAlias.findMatchingAliases(te, declaredType)
    if (aliases.size == 1) {
      val replaced = te.replace(createTypeElementFromText(aliases.head.canonicalText, te.getContext, te))
      TypeAdjuster.markToAdjust(replaced)
    } else {
      implicit val tpc: TypePresentationContext = TypePresentationContext(te)
      val texts                                 = aliases.sortBy(_.presentableText.length).map(ScTypeText(_))
      val expr                                  = new ChooseTypeTextExpression(texts, ScTypeText(declaredType))
      startTemplate(te, te.getParent, expr, editor)
    }
    true
  }

  override protected def shouldSuggest(te: ScTypeElement, declaredType: ScType): Boolean = {
    val aliases = SuggestTypeAlias.findMatchingAliases(te, declaredType)
    aliases.length > 1 && aliases.head != declaredType
  }
}

object SuggestTypeAlias {

  def allAliasesFor(tpe: ScType, context: ProjectContext, scope: GlobalSearchScope): List[ScTypeAlias] = {
    val qualifier = tpe.extractClass.flatMap {
      case t: ScTypeDefinition => Some(t.getPath)
      case _ => None
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
    }.flatten ++ topLevelType(te)).distinct

  def topLevelType(te: ScTypeElement): List[ScType] =
    te.`type`().toOption match {
      case Some(tpe) =>
        tpe.aliasType match {
          case Some(AliasType(_, _, Right(value))) => List(value)
          case _                                   => List(tpe)
        }
      case _ => Nil
    }
}
