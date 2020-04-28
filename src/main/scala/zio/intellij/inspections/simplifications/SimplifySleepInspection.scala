package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class SimplifySleepInspection extends ZInspection(SleepSimplificationType)

object SleepSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .delay"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, duration: ScExpression) =
      replace(expr).withText(invocationText(qual, s"delay(${duration.getText}")).highlightAll
    expr match {
      case `ZIO.sleep`(duration) `.*>` io                       => Some(replacement(io, duration))
      case `ZIO.sleep`(duration) `.flatMap` lambda(_, Some(io)) => Some(replacement(io, duration))
      case _                                                    => None
    }
  }
}
