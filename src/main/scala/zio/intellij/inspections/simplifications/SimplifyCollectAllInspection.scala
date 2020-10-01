package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.collectionMethods.`.map`
import zio.intellij.utils.StringUtils._
import zio.intellij.utils.types.ZioType

class SimplifyCollectAllInspection
    extends ZInspection(
      CollectAllToForeachSimplificationType,
      CollectAllParToForeachParSimplificationType,
      CollectAllParNToForeachParNSimplificationType
    )

sealed abstract class BaseCollectAllSimplificationType(methodName: String, methodExtractor: ZIOStaticMemberReference)
    extends SimplificationType {
  override def hint: String = s"Replace with ZIO.$methodName"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(zioType: ZioType, iterable: ScExpression, func: ScExpression) =
      replace(expr)
        .withText(s"${zioType.name}.$methodName${iterable.getWrappedText}${func.getWrappedText}")
        .highlightAll

    expr match {
      case methodExtractor(zioType, xs `.map` f) => Some(replacement(zioType, xs, f))
      case _                                     => None
    }
  }
}

object CollectAllToForeachSimplificationType
    extends BaseCollectAllSimplificationType(methodName = "foreach", methodExtractor = `ZIO.collectAll`)

object CollectAllParToForeachParSimplificationType
    extends BaseCollectAllSimplificationType(methodName = "foreachPar", methodExtractor = `ZIO.collectAllPar`)

object CollectAllParNToForeachParNSimplificationType extends SimplificationType {
  override def hint: String = s"Replace with ZIO.foreachParN"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(zioType: ZioType, n: ScExpression, iterable: ScExpression, func: ScExpression) =
      replace(expr)
        .withText(s"${zioType.name}.foreachParN${n.getWrappedText}${iterable.getWrappedText}${func.getWrappedText}")
        .highlightAll

    expr match {
      case `ZIO.collectAllParN`(zioType, n, iterable `.map` func) => Some(replacement(zioType, n, iterable, func))
      case _                                                      => None
    }
  }
}
