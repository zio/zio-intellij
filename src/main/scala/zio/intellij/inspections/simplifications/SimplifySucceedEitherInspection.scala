package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.scalaEitherExtractors._
import zio.intellij.inspections.simplifications.SimplifySucceedEitherInspection.UIOApply

class SimplifySucceedEitherInspection extends ZInspection(LeftSimplificationType, RightSimplificationType)

object LeftSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.left"

  private def replacement(expr: ScExpression, a: ScExpression): Simplification =
    replace(expr).withText(s"ZIO.left(${a.getText}").highlightFrom(expr)

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(scalaLeft(a)) => Some(replacement(expr, a))
      case UIOApply(scalaLeft(a))      => Some(replacement(expr, a))
      case _                           => None
    }
}

object RightSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.right"

  private def replacement(expr: ScExpression, a: ScExpression): Simplification =
    replace(expr).withText(s"ZIO.right(${a.getText}").highlightFrom(expr)

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(scalaRight(a)) => Some(replacement(expr, a))
      case UIOApply(scalaRight(a))      => Some(replacement(expr, a))
      case _                            => None
    }
}

object SimplifySucceedEitherInspection {
  val UIOApply = new Apply(Set("zio.UIO"))
}
