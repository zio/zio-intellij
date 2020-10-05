package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections.zioMethods._
import zio.intellij.inspections.{`ZIO.fail`, ZInspection}
import zio.intellij.utils.StringUtils.ScExpressionExt
import zio.intellij.utils.types.ZioType

class SimplifyFailInspection extends ZInspection(FailOrDieSimplificationType)

object FailOrDieSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.die"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(zioType: ZioType, error: ScExpression): Simplification =
      replace(expr).withText(s"${zioType.name}.die${error.getWrappedText}").highlightAll

    expr match {
      case `ZIO.fail`(zioType, error) `.orDie` () => Some(replacement(zioType, error))
      case _                                      => None
    }
  }
}
