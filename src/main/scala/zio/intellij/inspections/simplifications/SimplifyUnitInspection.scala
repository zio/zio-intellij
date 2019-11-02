package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections.{`.*>`, zioUnit, ZInspection}

class SimplifyUnitInspection extends ZInspection(UnitSimplificationType)

object UnitSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .unit"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual `.*>` `zioUnit`(_) =>
        Some(replace(expr).withText(invocationText(qual, "unit")))
      case _ => None
    }
}
