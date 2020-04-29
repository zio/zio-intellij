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

    def createFix(localFix: QuickFix): ProblemDescriptor =
      manager.createProblemDescriptor(
        localFix.toReplace,
        messageFormat.format(localFix.wrappedEffect, localFix.wrappedEffect),
        isOnTheFly,
        Array[LocalQuickFix](localFix),
        ProblemHighlightType.WEAK_WARNING
      )

    element match {
      case expr: ScExpression if IntentionAvailabilityChecker.checkInspection(this, expr.getParent) =>
        expr match {
          case FutureExpression(f) => Some(createFix(new QuickFix(expr, f, "Future", "implicit ec => ")))
          case TryExpression(f)    => Some(createFix(new QuickFix(expr, f, "Try")))
          case OptionExpression(f) => Some(createFix(new QuickFix(expr, f, "Option")))
          case EitherExpression(f) => Some(createFix(new QuickFix(expr, f, "Either")))
          case _                   => None
        }
      case _ => None
    }
  }

  val FutureExpression = new ExpressionExtractor(scalaFuture)
  val TryExpression    = new ExpressionExtractor(scalaTry)
  val OptionExpression = new ExpressionExtractor(scalaOption)
  val EitherExpression = new ExpressionExtractor(scalaEither)

  final class ExpressionExtractor(extractor: TypeReference) {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case `ZIO.apply`(extractor(f))       => Some(f)
      case `ZIO.effect`(extractor(f))      => Some(f)
      case `ZIO.effectTotal`(extractor(f)) => Some(f)
      case _                               => None
    }
  }
}

final class QuickFix(
  val toReplace: ScExpression,
  val replaceWith: ScExpression,
  val wrappedEffect: String,
  prefix: String = ""
) extends AbstractFixOnPsiElement(s"Replace with ZIO.from$wrappedEffect", toReplace) {

  override protected def doApplyFix(element: ScExpression)(implicit project: Project): Unit =
    element.replace(createExpressionFromText(s"ZIO.from$wrappedEffect($prefix${replaceWith.getText}"))
}

object WrapInsteadOfLiftInspection {
  val messageFormat = "Possibly mistaken wrapping of %s in a ZIO effect. Perhaps you meant to use ZIO.from%s?"
}
