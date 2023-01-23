package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{stripped, MethodRepr}
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import zio.intellij.inspections.mistakes.UnnecessaryEnvProvisionInspection.ProvideQuickFix
import zio.intellij.inspections.zioMethods.`.provideSomeLayer`
import zio.intellij.utils.TypeCheckUtils.isAnyEnvEffect
import zio.intellij.utils._

class UnnecessaryEnvProvisionInspection extends LocalInspectionTool {

  private def needsEnvDesignator(context: PsiElement): Option[ScType] =
    createType("_root_.zio.NeedsEnv", context)

  private def isNeedsEnvEv(element: PsiElement): Boolean =
    element match {
      case Typeable(ParameterizedType(designator, _)) => needsEnvDesignator(element).exists(_.equiv(designator))
      case _                                          => false
    }

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case MethodRepr(expr, Some(base), Some(ref), _) if isAnyEnvEffect(base) =>
      createPossibleFix(holder, expr, base, ref)
    case MethodRepr(expr @ `.provideSomeLayer`(_, _), Some(MethodRepr(_, Some(base), Some(ref), _)), _, _)
        if isAnyEnvEffect(base) =>
      createPossibleFix(holder, expr, base, ref)
    case _ =>
  }

  private def createPossibleFix(
    holder: ProblemsHolder,
    expr: ScExpression,
    base: ScExpression,
    toDelete: ScReference
  ): Unit =
    expr.findImplicitArguments.foreach { args =>
      if (args.map(_.element).exists(isNeedsEnvEv)) {
        holder.registerProblem(
          expr,
          getDisplayName,
          ProblemHighlightType.ERROR,
          new ProvideQuickFix(expr, base, toDelete.refName)
        )
      }
    }
}

object UnnecessaryEnvProvisionInspection {
  def hint(toDelete: String) = s"Remove unnecessary .$toDelete"
  final private class ProvideQuickFix(expr: ScExpression, base: ScExpression, toDelete: String)
      extends AbstractFixOnPsiElement(hint(toDelete), expr) {
    override protected def doApplyFix(expr: ScExpression)(implicit project: Project): Unit =
      expr.replace(stripped(base))
  }
}
