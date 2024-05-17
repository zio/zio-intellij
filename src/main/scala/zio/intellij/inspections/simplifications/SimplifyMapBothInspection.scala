package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.StringUtils._
import zio.intellij.utils.ZioVersion
import zio.intellij.utils.ZioVersion.ZIO

class SimplifyMapBothInspection extends ZInspection(MapBothSimplificationType) {
  override protected def isAvailable(zioVersion: ZioVersion): Boolean = zioVersion >= ZIO.`1.0.10`
}

object MapBothSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .mapBoth"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, a: ScExpression, b: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"mapBoth(${b.getBracedText}, ${a.getBracedText})"))
        .highlightFrom(qual)

    def toFunctionExpr(e: ScExpression): ScExpression =
      ScalaPsiElementFactory.createExpressionFromText(s"_ => ${e.getText}", e)(e)

    expr match {
      case qual `.map` a `.mapError` b   => Some(replacement(qual, a, b))
      case qual `.map` a `.orElseFail` b => Some(replacement(qual, a, toFunctionExpr(b)))
      case qual `.as` a `.mapError` b    => Some(replacement(qual, toFunctionExpr(a), b))
      case qual `.as` a `.orElseFail` b  => Some(replacement(qual, toFunctionExpr(a), toFunctionExpr(b)))
      case qual `.mapError` a `.map` b   => Some(replacement(qual, b, a))
      case qual `.mapError` a `.as` b    => Some(replacement(qual, toFunctionExpr(b), a))
      case qual `.orElseFail` a `.map` b => Some(replacement(qual, b, toFunctionExpr(a)))
      case qual `.orElseFail` a `.as` b  => Some(replacement(qual, toFunctionExpr(b), toFunctionExpr(a)))
      case _                             => None
    }
  }
}
