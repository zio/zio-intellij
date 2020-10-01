package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.utils.StringUtils._
import zio.intellij.utils.types.ZioType
import zio.intellij.utils.types.ZioTypes.{UIO, URIO}

class SimplifySucceedOptionInspection extends ZInspection(NoneSimplificationType, SomeSimplificationType)

object NoneSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.none"

  private def replacement(zioType: ZioType, expr: ScExpression): Simplification =
    replace(expr).withText(s"${zioType.name}.none").highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(zioType, scalaNone())              => Some(replacement(zioType, expr))
      case `ZIO.apply`(zioType @ (UIO | URIO), scalaNone()) => Some(replacement(zioType, expr))
      case _                                                => None
    }
}

object SomeSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.some"

  private def replacement(zioType: ZioType, expr: ScExpression, a: ScExpression): Simplification =
    replace(expr).withText(s"${zioType.name}.some${a.getWrappedText}").highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(zioType, scalaSome(a))              => Some(replacement(zioType, expr, a))
      case `ZIO.apply`(zioType @ (UIO | URIO), scalaSome(a)) => Some(replacement(zioType, expr, a))
      case _                                                 => None
    }
}
