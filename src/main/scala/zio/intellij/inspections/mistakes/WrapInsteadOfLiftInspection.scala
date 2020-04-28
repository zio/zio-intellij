package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, AbstractRegisteredInspection}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.inspections._
import zio.intellij.inspections.mistakes.WrapInsteadOfLiftInspection.messageFormat

class WrapInsteadOfLiftInspection extends AbstractRegisteredInspection {

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] = {

    def createFix(expr: ScExpression, wrappedEffect: String, localFix: LocalQuickFix): ProblemDescriptor =
      manager.createProblemDescriptor(
        expr,
        messageFormat.format(wrappedEffect, wrappedEffect),
        isOnTheFly,
        Array(localFix),
        ProblemHighlightType.WEAK_WARNING
      )

    element match {
      case expr: ScExpression if IntentionAvailabilityChecker.checkInspection(this, expr.getParent) =>
        expr match {
          case Future(f) => Some(createFix(expr, "Future", new FutureToZio(expr, f)))
          case _         => None
        }
      case _ => None
    }
  }

  object Future {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case `ZIO`(scalaFuture(f))             => Some(f)
      case `ZIO.apply`(scalaFuture(f))       => Some(f)
      case `ZIO.effect`(scalaFuture(f))      => Some(f)
      case `ZIO.effectTotal`(scalaFuture(f)) => Some(f)
      case _                                 => None
    }
  }
}

final class FutureToZio(toReplace: ScExpression, replaceWith: ScExpression)
    extends AbstractFixOnPsiElement("Replace with ZIO.fromFuture", toReplace) {

  override protected def doApplyFix(element: ScExpression)(implicit project: Project): Unit =
    element.replace(createExpressionFromText(s"ZIO.fromFuture(implicit ec => ${replaceWith.getText}"))
}

object WrapInsteadOfLiftInspection {
  val messageFormat = "Possibly mistaken wrapping of %s in a ZIO effect. Perhaps you meant to use ZIO.from%s?"
}
