package zio.intellij.inspections.suggestions

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{ScType, TypePresentationContext}
import zio.intellij.inspections.suggestions.SuggestTypeAliasInspection.{AliasInfo, TypeAliasQuickFix}
import zio.intellij.intentions.suggestions.SuggestTypeAlias
import zio.intellij.utils.TypeCheckUtils.{fromZioLayer, fromZioLike}
import zio.intellij.utils.{ListSyntax, createTypeElement, extractTypeArguments}

class SuggestTypeAliasInspection extends LocalInspectionTool {

  private def shouldSuggest(declaredType: ScType, aliases: List[AliasInfo]): Boolean =
    extractTypeArguments(declaredType)
      .map(_.length)
      .exists(args => aliases.exists(_.args < args))

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case te: ScTypeElement if isOnTheFly =>
      te match {
        case Typeable(tpe) if fromZioLike(tpe) || fromZioLayer(tpe) =>
          val allAliases = SuggestTypeAlias.findMatchingAliases(te, tpe)
          val mostSpecificAliases =
            allAliases
              .flatMap(alias => extractTypeArguments(alias).map(_.length).map(AliasInfo(alias, _)))
              .minsBy(_.args)

          Option.when(shouldSuggest(tpe, mostSpecificAliases)) {
            implicit val tpc: TypePresentationContext = TypePresentationContext(te)

            val typeElements = mostSpecificAliases.flatMap(alias => createTypeElement(alias.tpe, te))
            holder.registerProblem(
              te,
              getDisplayName,
              ProblemHighlightType.INFORMATION,
              typeElements.map(new TypeAliasQuickFix(te, _)): _*
            )
          }
        case _ =>
      }
    case _ =>
  }
}

object SuggestTypeAliasInspection {

  def hint(to: String): String = s"Replace with a more specific $to"

  final private class TypeAliasQuickFix(from: ScTypeElement, to: ScTypeElement)
      extends AbstractFixOnPsiElement(hint(to.getText), from) {
    override protected def doApplyFix(expr: ScTypeElement)(implicit project: Project): Unit =
      expr.replace(to)
  }

  final private case class AliasInfo(tpe: ScType, args: Int)
}
