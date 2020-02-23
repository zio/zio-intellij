package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.collectionMethods.`.map`

class SimplifyCollectAllInspection
    extends ZInspection(CollectAllToForeachSimplificationType, CollectAllParToForeachParSimplificationType)

sealed abstract class BaseCollectAllSimplificationType(methodName: String, methodExtractor: ZIOMemberReference)
    extends SimplificationType {
  override def hint: String = s"Replace with ZIO.$methodName"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(iterable: ScExpression, func: ScExpression) =
      replace(expr).withText(s"ZIO.$methodName(${iterable.getText})(${func.getText})").highlightFrom(expr)

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
