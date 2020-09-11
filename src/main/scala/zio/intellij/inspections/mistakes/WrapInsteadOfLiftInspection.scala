package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, AbstractRegisteredInspection}
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenerator}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScValueOrVariable
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createExpressionFromText
import zio.intellij.inspections._
import zio.intellij.inspections.mistakes.WrapInsteadOfLiftInspection.messageFormat
import zio.intellij.utils._

import scala.annotation.tailrec

class WrapInsteadOfLiftInspection extends AbstractRegisteredInspection {

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] = {

    @tailrec
    def isUsed(expr: Option[PsiElement]): Boolean = {
      val _isUsed: PsiElement => Boolean = {
        case n: ScNamedElement => isElementUsed(n, isOnTheFly)
        case _                 => false
      }

      expr match {
        case Some(v: ScValueOrVariable) =>
          v.hasExplicitType || v.declaredElements.exists(_isUsed)
        case Some(r: ScReferencePattern) =>
          r.bindings.filterNot(_.isWildcard).exists(_isUsed)
        case Some(ScGenerator(p)) => isUsed(p.toOption.map(_._1))
        case _                    => false
      }
    }

    def createFix(localFix: QuickFix): ProblemDescriptor =
      manager.createProblemDescriptor(
        localFix.toReplace,
        messageFormat.format(localFix.wrappedEffect, localFix.wrappedEffect),
        isOnTheFly,
        Array[LocalQuickFix](localFix),
        ProblemHighlightType.WEAK_WARNING
      )

    element match {
      case expr: ScExpression =>
        expr match {
          case Future(f) if !isUsed(expr.parent) => Some(createFix(new QuickFix(expr, f, "Future", "implicit ec => ")))
          case Try(f) if !isUsed(expr.parent)    => Some(createFix(new QuickFix(expr, f, "Try")))
          case Option(f) if !isUsed(expr.parent) => Some(createFix(new QuickFix(expr, f, "Option")))
          case Either(f) if !isUsed(expr.parent) => Some(createFix(new QuickFix(expr, f, "Either")))
          case _                                 => None
        }
      case _ => None
    }
  }

  val Future = new ExpressionExtractor(scalaFuture)
  val Try    = new ExpressionExtractor(scalaTry)
  val Option = new ExpressionExtractor(scalaOption)
  val Either = new ExpressionExtractor(scalaEither)

  final class ExpressionExtractor(extractor: ReturnTypeReference) {

    def unapply(expr: ScExpression): Option[ScExpression] =
      expr match {
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
