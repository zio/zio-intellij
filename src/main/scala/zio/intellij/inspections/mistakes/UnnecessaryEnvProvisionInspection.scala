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
import zio.intellij.inspections.mistakes.UnnecessaryEnvProvisionInspection.createFix
import zio.intellij.inspections.zioMethods.`.provideSomeLayer`
import zio.intellij.utils.TypeCheckUtils.isAnyEnvEffect
import zio.intellij.utils.{OptionUtils => OptionOps, _}

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
      createPossibleFix(holder.getManager, isOnTheFly, expr, base, ref)
    case MethodRepr(expr @ `.provideSomeLayer`(_, _), Some(MethodRepr(_, Some(base), Some(ref), _)), _, _)
        if isAnyEnvEffect(base) =>
      createPossibleFix(holder.getManager, isOnTheFly, expr, base, ref)
    case _ => None
  }

  private def createPossibleFix(
    manager: InspectionManager,
    isOnTheFly: Boolean,
    expr: ScExpression,
    base: ScExpression,
    ref: ScReference
  ) =
    expr.findImplicitArguments.flatMap { args =>
      OptionOps.when(args.map(_.element).exists(isNeedsEnvEv)) {
        createFix(manager, isOnTheFly, expr, base, ref, getDisplayName)
      }
    }
}

object UnnecessaryEnvProvisionInspection {
  def hint(toDelete: String) = s"Remove unnecessary .$toDelete"

  def createFix(
    manager: InspectionManager,
    isOnTheFly: Boolean,
    expr: ScExpression,
    base: ScExpression,
    toDelete: ScReference,
    description: String
  ): ProblemDescriptor =
    manager.createProblemDescriptor(
      expr,
      description,
      isOnTheFly,
      Array[LocalQuickFix](new ProvideQuickFix(expr, base, toDelete.refName)),
      ProblemHighlightType.ERROR
    )

  final private class ProvideQuickFix(expr: ScExpression, base: ScExpression, toDelete: String)
      extends AbstractFixOnPsiElement(hint(toDelete), expr) {
    override protected def doApplyFix(expr: ScExpression)(implicit project: Project): Unit =
      expr.replace(stripped(base))
  }
}
