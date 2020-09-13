package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.collectionMethods.`.map`
import zio.intellij.utils.StringUtils._

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
    def replacement(iterable: ScExpression, func: ScExpression) =
      replace(expr).withText(s"ZIO.$methodName${iterable.getWrappedText}${func.getWrappedText}").highlightAll

    expr match {
      case methodExtractor(xs `.map` f) => Some(replacement(xs, f))
      case _                            => None
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
    def replacement(n: ScExpression, iterable: ScExpression, func: ScExpression) =
      replace(expr)
        .withText(s"ZIO.foreachParN${n.getWrappedText}${iterable.getWrappedText}${func.getWrappedText}")
        .highlightAll

    expr match {
      case `ZIO.collectAllParN`(n, iterable `.map` func) => Some(replacement(n, iterable, func))
      case _                                             => None
    }
  }
}
