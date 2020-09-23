package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.utils.TypeCheckUtils._

class SimplifyToLayerInspection
    extends ZInspection(ZLayerFromEffectToLayerSimplificationType, ZLayerFromEffectManyToLayerManySimplificationType)

sealed abstract class BaseToLayerSimplificationType(methodName: String, methodExtractor: ZLayerStaticMemberReference)
    extends SimplificationType {
  override def hint: String = s"Replace with .$methodName"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case methodExtractor(effect) if fromZio(effect) =>
        Some(replace(expr).withText(invocationText(effect, methodName)).highlightFrom(expr))
      case _ => None
    }
}

object ZLayerFromEffectToLayerSimplificationType
    extends BaseToLayerSimplificationType(methodName = "toLayer", methodExtractor = `ZLayer.fromEffect`)

object ZLayerFromEffectManyToLayerManySimplificationType
    extends BaseToLayerSimplificationType(methodName = "toLayerMany", methodExtractor = `ZLayer.fromEffectMany`)
