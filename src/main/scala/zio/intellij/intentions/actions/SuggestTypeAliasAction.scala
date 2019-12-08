package zio.intellij.intentions.actions

import com.intellij.openapi.editor.Editor
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.codeInsight.intention.types.{startTemplate, ChooseTypeTextExpression}
import org.jetbrains.plugins.scala.lang.psi.TypeAdjuster
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScTypeAlias, ScTypeAliasDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager
import org.jetbrains.plugins.scala.lang.psi.types.api.designator.ScDesignatorType
import org.jetbrains.plugins.scala.lang.psi.types.api.{ParameterizedType, ScTypeText, UndefinedType}
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.project.ProjectContext
import zio.intellij.intentions.ZTypeAnnotationIntention

final class SuggestTypeAliasAction extends ZTypeAnnotationIntention {

  override def getFamilyName: String = "Choose a more specific ZIO type alias"

  def computeAliases(context: ProjectContext, scope: GlobalSearchScope): List[ScTypeAlias] = {
    val manager = ScalaPsiManager.instance(context)
    manager.getCachedClass(scope, "zio") match {
      case Some(z: ScObject) => z.aliases.toList
      case _                 => Nil
    }
  }

  override protected def invoke(te: ScTypeElement, declaredType: ScType, editor: Editor): Boolean = {
    val aliases = (computeAliases(te.projectContext, te.getResolveScope).flatMap {
      case alias: ScTypeAliasDefinition =>
        val undefParams = alias.typeParameters.map(UndefinedType(_))
        val undefSubst  = ScSubstitutor.bind(alias.typeParameters, undefParams)
        for {
          aliasType <- alias.aliasedType.toOption
          subst     <- undefSubst(aliasType).conformanceSubstitutor(declaredType)
        } yield subst.apply(ParameterizedType(ScDesignatorType(alias), undefParams))
    } ++ topLevelType(te)).distinct
      .sortBy(_.canonicalText.length)

    if (aliases.size == 1) {
      val replaced = te.replace(createTypeElementFromText(aliases.head.canonicalText, te.getContext, te))
      TypeAdjuster.markToAdjust(replaced)
    } else {
      implicit val tpc: TypePresentationContext = TypePresentationContext(te)

      val texts = aliases.map(ScTypeText(_))
      val expr  = new ChooseTypeTextExpression(texts, ScTypeText(declaredType))
      startTemplate(te, te.getParent, expr, editor)
    }

    true
  }

  def topLevelType(te: ScTypeElement): List[ScType] =
    te.`type`().toOption match {
      case Some(typ) =>
        typ.isAliasType match {
          case Some(AliasType(_, _, Right(value))) => List(value)
          case _                                   => List(typ)
        }
      case _ => Nil
    }
}
