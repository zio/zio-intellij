package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFor}
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods.`.*>`
import zio.intellij.utils.StringUtils._
import zio.intellij.utils.ZioVersion
import zio.intellij.utils.types.ZioType

class SimplifyForeachInspectionZIO1
    extends ZInspection(
      ForeachForCompSimplificationType("foreach_"),
      ForeachParForCompSimplificationType("foreachPar_"),
      ForeachParNForCompSimplificationType,
      ForeachChainSimplificationType("foreach_"),
      ForeachParChainSimplificationType("foreachPar_"),
      ForeachParNChainSimplificationType
    ) {
  override protected def isAvailable(zioVersion: ZioVersion): Boolean =
    zioVersion >= ZioVersion.ZIO.`1.0.0` && zioVersion < ZioVersion.ZIO.`2.0.0`
}

class SimplifyForeachInspectionZIO2
    extends ZInspection(
      ForeachForCompSimplificationType("foreachDiscard"),
      ForeachParForCompSimplificationType("foreachParDiscard"),
      ForeachChainSimplificationType("foreachDiscard"),
      ForeachParChainSimplificationType("foreachParDiscard")
    ) {
  override protected def isAvailable(zioVersion: ZioVersion): Boolean = zioVersion >= ZioVersion.ZIO.`2.0.0`
}

sealed abstract class BaseForeachSimplificationType(methodName: String) extends SimplificationType {

  override def hint: String = s"Replace with ZIO.$methodName"

  protected def replacement(
    zioType: ZioType,
    expr: ScExpression,
    iterable: ScExpression,
    func: ScExpression
  ): Simplification =
    replace(expr).withText(s"${zioType.name}.$methodName${iterable.getWrappedText}${func.getWrappedText}").highlightAll
}

sealed abstract class BaseForeachParNSimplificationType extends SimplificationType {

  private val methodName = "foreachParN_"

  override def hint: String = s"Replace with ZIO.$methodName"

  def replacement(
    zioType: ZioType,
    expr: ScExpression,
    n: ScExpression,
    iterable: ScExpression,
    func: ScExpression
  ): Simplification =
    replace(expr)
      .withText(s"${zioType.name}.$methodName${n.getWrappedText}${iterable.getWrappedText}${func.getWrappedText}")
      .highlightAll
}

sealed abstract class BaseForeachForCompSimplificationType(
  methodName: String,
  methodExtractor: ZIOCurried2StaticMemberReference
) extends BaseForeachSimplificationType(methodName) {

  override def getSimplifications(expr: ScExpression): Seq[Simplification] =
    expr match {
      case ScFor(enumerators, _) =>
        enumerators.generators.collect {
          case `_ <- x`(expr @ methodExtractor(zioType, iterable, func)) => replacement(zioType, expr, iterable, func)
        }.toSeq
      case _ => Nil
    }
}

case class ForeachForCompSimplificationType(methodName: String)
    extends BaseForeachForCompSimplificationType(methodName = methodName, methodExtractor = `ZIO.foreach`)

case class ForeachParForCompSimplificationType(methodName: String)
    extends BaseForeachForCompSimplificationType(methodName = methodName, methodExtractor = `ZIO.foreachPar`)

object ForeachParNForCompSimplificationType extends BaseForeachParNSimplificationType {

  override def getSimplifications(expr: ScExpression): Seq[Simplification] =
    expr match {
      case ScFor(enumerators, _) =>
        enumerators.generators.collect {
          case `_ <- x`(expr @ `ZIO.foreachParN`(zioType, n, iterable, func)) =>
            replacement(zioType, expr, n, iterable, func)
        }.toSeq
      case _ => Nil
    }
}

sealed abstract class BaseForeachChainSimplificationType(
  methodName: String,
  methodExtractor: ZIOCurried2StaticMemberReference
) extends BaseForeachSimplificationType(methodName) {

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case (expr @ methodExtractor(zioType, iterable, func)) `.*>` _ => Some(replacement(zioType, expr, iterable, func))
      case _ `.*>` (expr @ methodExtractor(zioType, iterable, func)) `.*>` _ =>
        Some(replacement(zioType, expr, iterable, func))
      case _ => None
    }
}

case class ForeachChainSimplificationType(methodName: String)
    extends BaseForeachChainSimplificationType(methodName = methodName, methodExtractor = `ZIO.foreach`)

case class ForeachParChainSimplificationType(methodName: String)
    extends BaseForeachChainSimplificationType(methodName = methodName, methodExtractor = `ZIO.foreachPar`)

object ForeachParNChainSimplificationType extends BaseForeachParNSimplificationType {

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case (expr @ `ZIO.foreachParN`(zioType, n, iterable, func)) `.*>` _ =>
        Some(replacement(zioType, expr, n, iterable, func))
      case _ `.*>` (expr @ `ZIO.foreachParN`(zioType, n, iterable, func)) `.*>` _ =>
        Some(replacement(zioType, expr, n, iterable, func))
      case _ => None
    }
}
