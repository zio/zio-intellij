package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class SimplifyBimapInspection extends ZInspection(BimapSimplificationType)

object BimapSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .bimap"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, a: ScExpression, b: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"bimap(${b.getText}, ${a.getText})"))
        .highlightFrom(qual)

    expr match {
      case qual `.map` a `.mapError` b => Some(replacement(qual, a, b))
      case qual `.map` a `.asError` b  => Some(replacement(qual, a, b))
      case qual `.as` a `.mapError` b  => Some(replacement(qual, a, b))
      case qual `.as` a `.asError` b   => Some(replacement(qual, a, b))
      case qual `.mapError` a `.map` b => Some(replacement(qual, b, a))
      case qual `.mapError` a `.as` b  => Some(replacement(qual, b, a))
      case qual `.asError` a `.map` b  => Some(replacement(qual, b, a))
      case qual `.asError` a `.as` b   => Some(replacement(qual, b, a))
      case _                           => None
    }
  }
}
