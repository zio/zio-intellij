package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScUnitExpr}
import zio.intellij.inspections._
import zio.intellij.utils.types.ZioType

class SimplifySucceedUnitInspection extends ZInspection(SucceedUnitSimplificationType)

object SucceedUnitSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.unit"

  private def replacement(zioType: ZioType, expr: ScExpression): Simplification =
    replace(expr).withText(s"${zioType.name}.unit").highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(zioType, scalaUnit())     => Some(replacement(zioType, expr))
      case `ZIO.apply`(zioType, scalaUnit())       => Some(replacement(zioType, expr))
      case `ZIO.attempt`(zioType, scalaUnit())     => Some(replacement(zioType, expr))
      case `ZIO.effect`(zioType, scalaUnit())      => Some(replacement(zioType, expr))
      case `ZIO.effectTotal`(zioType, scalaUnit()) => Some(replacement(zioType, expr))
      case _                                       => None
    }

  private object scalaUnit {
    def unapply(expr: ScExpression): Boolean =
      expr match {
        case _: ScUnitExpr => true
        case _             => false
      }
  }

}
