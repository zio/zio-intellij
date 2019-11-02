package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._

class SimplifyUnitInspection extends ZInspection(UnitSimplificationType)

object UnitSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .unit"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression) = replace(expr).withText(invocationText(qual, "unit"))
    expr match {
      case qual `.*>` zioUnit()      => Some(replacement(qual)) // *> ZIO.unit
      case qual `.as` unitLiteral()  => Some(replacement(qual)) // .as(())
      case qual `.map` returnsUnit() => Some(replacement(qual)) // .map(_ => ())
      case _                         => None
    }
  }

}
