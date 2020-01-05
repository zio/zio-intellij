package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._

class SimplifySucceedOptionInspection extends ZInspection(NoneSimplificationType, SomeSimplificationType)

object NoneSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.none"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(scalaNone()) => Some(replace(expr).withText("ZIO.none").highlightFrom(expr))
      case _                          => None
    }
}

object SomeSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.some"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(scalaSome(a)) => Some(replace(expr).withText(s"ZIO.some(${a.getText}").highlightFrom(expr))
      case _                           => None
    }

}
