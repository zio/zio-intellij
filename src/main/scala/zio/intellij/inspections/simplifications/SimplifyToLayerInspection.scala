package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFor}
import zio.intellij.inspections._
import zio.intellij.utils.TypeCheckUtils._
import org.jetbrains.plugins.scala.extensions._

class SimplifyToLayerInspection
    extends ZInspection(ZLayerFromEffectToLayerSimplificationType, ZLayerFromEffectManyToLayerManySimplificationType)

sealed abstract class BaseToLayerSimplificationType(methodName: String, methodExtractor: ZLayerStaticMemberReference)
    extends SimplificationType {
  override def hint: String = s"Replace with .$methodName"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case methodExtractor(_, effect) if fromZio(effect) =>
        val replacementText = effect match {
          case _: ScFor => s"${effect.getText.parenthesize(true)}.$methodName"
          case _        => invocationText(effect, methodName)
        }
        Some(replace(expr).withText(replacementText).highlightFrom(expr))
      case _ => None
    }
}

object ZLayerFromEffectToLayerSimplificationType
    extends BaseToLayerSimplificationType(methodName = "toLayer", methodExtractor = `ZLayer.fromEffect`)

object ZLayerFromEffectManyToLayerManySimplificationType
    extends BaseToLayerSimplificationType(methodName = "toLayerMany", methodExtractor = `ZLayer.fromEffectMany`)
