package zio.intellij.inspections

import com.intellij.codeInspection.{ InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType }
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.AbstractRegisteredInspection
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker

class UnusedZIOExpressionsInspection extends AbstractRegisteredInspection {

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case expr: ScExpression if IntentionAvailabilityChecker.checkInspection(this, expr.getParent) =>
        (expr, Option(expr.getNextSiblingNotWhitespace)) match {
          case (zioRef(ref1, _), Some(zioRef(_, _))) =>
            val message = "This expression is unused. Did you mean to compose it with another effect?"
            Some(
              manager.createProblemDescriptor(
                expr,
                message,
                isOnTheFly,
                Array.empty[LocalQuickFix],
                ProblemHighlightType.LIKE_UNUSED_SYMBOL
              )
            )
          case _ => None
        }
      case _ => None
    }
}
