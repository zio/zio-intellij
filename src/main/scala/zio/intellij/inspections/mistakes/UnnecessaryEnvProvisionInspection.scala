package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.{stripped, MethodRepr}
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, AbstractRegisteredInspection}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import zio.intellij.inspections.mistakes.UnnecessaryEnvProvisionInspection.createFix
import zio.intellij.inspections.zioMethods.`.provideSomeLayer`
import zio.intellij.utils.TypeCheckUtils.isAnyEnvEffect
import zio.intellij.utils.{OptionUtils => OptionOps, _}

class UnnecessaryEnvProvisionInspection extends AbstractRegisteredInspection {

  private def needsEnvDesignator(context: PsiElement): Option[ScType] =
    createType("_root_.zio.NeedsEnv", context)

  private def isNeedsEnvEv(element: PsiElement): Boolean =
    element match {
      case Typeable(ParameterizedType(designator, _)) => needsEnvDesignator(element).exists(_.equiv(designator))
      case _                                          => false
    }

  private def createPossibleFix(
    expr: ScExpression,
    base: ScExpression,
    ref: ScReference,
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    expr.findImplicitArguments.flatMap { args =>
      OptionOps.when(args.map(_.element).exists(isNeedsEnvEv)) {
        createFix(expr, base, ref, descriptionTemplate, highlightType)
      }
    }

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case MethodRepr(expr, Some(base), Some(ref), _) if isAnyEnvEffect(base) =>
        createPossibleFix(expr, base, ref, descriptionTemplate, highlightType)
      case MethodRepr(expr @ `.provideSomeLayer`(_, _), Some(MethodRepr(_, Some(base), Some(ref), _)), _, _)
          if isAnyEnvEffect(base) =>
        createPossibleFix(expr, base, ref, descriptionTemplate, highlightType)
      case _ => None
    }
}

object UnnecessaryEnvProvisionInspection {
  def hint(toDelete: String) = s"Remove unnecessary .$toDelete"

  def createFix(
    expr: ScExpression,
    base: ScExpression,
    toDelete: ScReference,
    description: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): ProblemDescriptor =
    manager.createProblemDescriptor(
      expr,
      description,
      isOnTheFly,
      Array[LocalQuickFix](new ProvideQuickFix(expr, base, toDelete.refName)),
      highlightType
    )

  final private class ProvideQuickFix(expr: ScExpression, base: ScExpression, toDelete: String)
      extends AbstractFixOnPsiElement(hint(toDelete), expr) {
    override protected def doApplyFix(expr: ScExpression)(implicit project: Project): Unit =
      expr.replace(stripped(base))
  }
}
