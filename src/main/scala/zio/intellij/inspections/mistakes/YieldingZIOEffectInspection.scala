package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.AbstractRegisteredInspection
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.inspections.zioRef

class YieldingZIOEffectInspection extends AbstractRegisteredInspection {

  private def createDescriptor(
    expr: ScExpression
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): ProblemDescriptor =
    manager.createProblemDescriptor(
      expr,
      YieldingZIOEffectInspection.message,
      isOnTheFly,
      Array.empty[LocalQuickFix],
      ProblemHighlightType.WEAK_WARNING
    )

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case expr: ScFor if IntentionAvailabilityChecker.checkInspection(this, expr.getParent) =>
        expr.body match {
          case Some(body @ zioRef(_, _)) => Some(createDescriptor(body))
          case Some(e: ScBlock) =>
            e.exprs.lastOption match {
              case Some(body @ zioRef(_, _)) => Some(createDescriptor(body))
              case None                      => None
            }
          case _ => None
        }
      case _ => None
    }
}

object YieldingZIOEffectInspection {
  val message = "Possibly mistaken yielding of a ZIO effect. Perhaps you meant to yield the result instead?"
}
