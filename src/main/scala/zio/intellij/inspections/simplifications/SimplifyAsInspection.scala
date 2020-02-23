package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class SimplifyAsInspection extends ZInspection(AsSimplificationType, AsErrorSimplificationType)

object AsSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .as"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, arg: ScExpression) =
      replace(expr).withText(invocationText(qual, s"as(${arg.getText}"))
    expr match {
      case qual `.map` `_ => x`(x) => Some(replacement(qual, x))
      case _                       => None
    }
  }
}

object AsErrorSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .asError"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, arg: ScExpression) =
      replace(expr).withText(invocationText(qual, s"asError(${arg.getText}"))
    expr match {
      case qual `.mapError` `_ => x`(x) => Some(replacement(qual, x))
      case _                            => None
    }
  }
}
