package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFor}
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods.`.*>`
import zio.intellij.utils.StringUtils._
import zio.intellij.utils.types.ZioType

class SimplifyForeachInspection
    extends ZInspection(
      ForeachForCompSimplificationType,
      ForeachParForCompSimplificationType,
      ForeachParNForCompSimplificationType,
      ForeachChainSimplificationType,
      ForeachParChainSimplificationType,
      ForeachParNChainSimplificationType
    )

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

object ForeachForCompSimplificationType
    extends BaseForeachForCompSimplificationType(methodName = "foreach_", methodExtractor = `ZIO.foreach`)

object ForeachParForCompSimplificationType
    extends BaseForeachForCompSimplificationType(methodName = "foreachPar_", methodExtractor = `ZIO.foreachPar`)

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

object ForeachChainSimplificationType
    extends BaseForeachChainSimplificationType(methodName = "foreach_", methodExtractor = `ZIO.foreach`)

object ForeachParChainSimplificationType
    extends BaseForeachChainSimplificationType(methodName = "foreachPar_", methodExtractor = `ZIO.foreachPar`)

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
