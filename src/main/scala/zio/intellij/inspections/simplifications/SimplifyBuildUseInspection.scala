package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{invocationText, Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression, ScUnderscoreSection}
import zio.intellij.inspections.layerMethods.`.build`
import zio.intellij.inspections.managedMethods.`.use`
import zio.intellij.inspections.zioMethods.`.provide`
import zio.intellij.inspections.{lambda, ZInspection}

class SimplifyBuildUseInspection extends ZInspection(BuildUseSimplificationType)

object BuildUseSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .provideLayer"

  def replacement(expr: ScExpression, zio: ScExpression, layer: ScExpression): Simplification =
    replace(expr).withText(invocationText(zio, "provideLayer", layer)).highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `.use`(`.build`(layer), body) =>
        body match {
          // layer.build.use(zio.provide)
          case `.provide`(zio) => Some(replacement(expr, zio, layer))
          // layer.build.use(zio.provide(_))
          case `.provide`(zio, _: ScUnderscoreSection) => Some(replacement(expr, zio, layer))
          // layer.build.use(l => zio.provide(l))
          case lambda(Seq(l), `.provide`(zio, ScReferenceExpression(lRef))) if l == lRef =>
            Some(replacement(expr, zio, layer))
          case _ => None
        }
      case _ => None
    }
}
