package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class SimplifyUnitInspection extends ZInspection(UnitSimplificationType)

object UnitSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .unit"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression) = replace(expr).withText(invocationText(qual, "unit"))
    expr match {
      case qual `.*>` `ZIO.unit`(_) => Some(replacement(qual))
      case qual `.as` `()`()        => Some(replacement(qual))
      case qual `.map` `_ => ()`()  => Some(replacement(qual))
      case _                        => None
    }
  }

}
