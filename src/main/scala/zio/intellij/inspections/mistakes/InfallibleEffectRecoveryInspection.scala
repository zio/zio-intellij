package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.MethodRepr
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, AbstractRegisteredInspection}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import zio.intellij.inspections.mistakes.InfallibleEffectRecoveryInspection._
import zio.intellij.utils.TypeCheckUtils.isInfallibleEffect
import zio.intellij.utils.{OptionUtils => OptionOps, _}

class InfallibleEffectRecoveryInspection extends AbstractRegisteredInspection {

  private def canFailDesignator(context: PsiElement): Option[ScType] =
    createType("_root_.zio.CanFail", context)

  private def isCanFailEv(element: PsiElement): Boolean =
    element match {
      case Typeable(ParameterizedType(designator, _)) => canFailDesignator(element).exists(_.equiv(designator))
      case _                                          => false
    }

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case MethodRepr(expr, Some(base), Some(ref), _) if isInfallibleEffect(base) =>
        expr.findImplicitArguments.flatMap { args =>
          OptionOps.when(args.map(_.element).exists(isCanFailEv))(createFix(expr, base, ref))
        }
      case _ => None
    }
}

object InfallibleEffectRecoveryInspection {
  def hint(toDelete: String) = s"Remove unreachable .$toDelete"

  def description(toDelete: String) =
    s"Effect cannot fail; operation .$toDelete is impossible"

  def createFix(
    expr: ScExpression,
    base: ScExpression,
    toDelete: ScReference
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): ProblemDescriptor =
    manager.createProblemDescriptor(
      expr,
      description(toDelete.refName),
      isOnTheFly,
      Array[LocalQuickFix](new RecoveryQuickFix(expr, base, toDelete.refName)),
      ProblemHighlightType.GENERIC_ERROR
    )

  final private class RecoveryQuickFix(expr: ScExpression, base: ScExpression, toDelete: String)
      extends AbstractFixOnPsiElement(hint(toDelete), expr) {
    override protected def doApplyFix(expr: ScExpression)(implicit project: Project): Unit =
      expr.replace(base)
  }
}
