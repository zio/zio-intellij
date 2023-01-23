package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.collections.{stripped, MethodRepr}
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import zio.intellij.inspections.mistakes.InfallibleEffectRecoveryInspection._
import zio.intellij.utils.TypeCheckUtils.isInfallibleEffect
import zio.intellij.utils._

class InfallibleEffectRecoveryInspection extends LocalInspectionTool {

  private def canFailDesignator(context: PsiElement): Option[ScType] =
    createType("_root_.zio.CanFail", context)

  private def isCanFailEv(element: PsiElement): Boolean =
    element match {
      case Typeable(ParameterizedType(designator, _)) => canFailDesignator(element).exists(_.equiv(designator))
      case _                                          => false
    }

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case MethodRepr(expr, Some(base), Some(toDelete), _) if isInfallibleEffect(base) =>
      expr.findImplicitArguments.foreach { args =>
        if (args.map(_.element).exists(isCanFailEv)) {
          holder.registerProblem(
            expr,
            description(toDelete.refName),
            ProblemHighlightType.GENERIC_ERROR,
            new RecoveryQuickFix(expr, base, toDelete.refName)
          )
        }
      }
    case _ =>
  }
}

object InfallibleEffectRecoveryInspection {
  def hint(toDelete: String) = s"Remove unreachable .$toDelete"

  @Nls
  def description(toDelete: String) =
    s"Effect cannot fail; operation .$toDelete is impossible"

  final private class RecoveryQuickFix(expr: ScExpression, base: ScExpression, toDelete: String)
      extends AbstractFixOnPsiElement(hint(toDelete), expr) {
    override protected def doApplyFix(expr: ScExpression)(implicit project: Project): Unit =
      expr.replace(stripped(base))
  }
}
