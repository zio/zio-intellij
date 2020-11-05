package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.AbstractRegisteredInspection
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import zio.intellij.inspections.zioLike

class UnusedZIOExpressionsInspection extends AbstractRegisteredInspection {

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case expr: ScExpression =>
        (expr, expr.nextSiblingNotWhitespace) match {
          case (zioLike(_), Some(zioLike(_))) =>
            Some(
              manager.createProblemDescriptor(
                expr,
                UnusedZIOExpressionsInspection.message,
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

object UnusedZIOExpressionsInspection {
  val message = "This expression is unused. Did you mean to compose it with another effect?"
}
