package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{invocationText, Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class SimplifyExitCodeInspection extends ZInspection(ExitCodeSimplificationType)

object ExitCodeSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .exitCode"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression) =
      replace(expr).withText(invocationText(qual, s"exitCode"))

    expr match {
      case qual `.map` `_ => x`(exitCodeSuccess() | exitCodeFailure())             => Some(replacement(qual))
      case qual `.as` (exitCodeSuccess() | exitCodeFailure())                      => Some(replacement(qual))
      case qual `.fold` (`_ => x`(exitCodeFailure()), `_ => x`(exitCodeSuccess())) => Some(replacement(qual))
      case _                                                                       => None
    }
  }
}
