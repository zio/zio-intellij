package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.StringUtils._

class SimplifyMapInspection extends ZInspection(AsSimplificationType, MapErrorSimplificationType)

object AsSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .as"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, arg: ScExpression) =
      replace(expr).withText(invocationTextFor(qual, s"as${arg.getWrappedText}"))
    expr match {
      case qual `.map` `_ => x`(x) => Some(replacement(qual, x))
      case _                       => None
    }
  }
}

object MapErrorSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .orElseFail"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, arg: ScExpression) =
      replace(expr).withText(invocationTextFor(qual, s"orElseFail${arg.getWrappedText}"))
    expr match {
      case qual `.mapError` `_ => x`(x) => Some(replacement(qual, x))
      case _                            => None
    }
  }
}
