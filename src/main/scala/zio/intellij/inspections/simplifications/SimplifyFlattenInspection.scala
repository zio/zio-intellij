package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{invocationText, Simplification, SimplificationType}
import org.jetbrains.plugins.scala.extensions.BooleanExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import zio.intellij.inspections.ZInspection
import zio.intellij.inspections.zioMethods._

class SimplifyFlattenInspection extends ZInspection(MapFlattenInspection)

object MapFlattenInspection extends SimplificationType {
  override def hint: String = "Replace .map and .flatten with .flatMap"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `.flatten`(qual `.map` f) =>
        val newText = invocationText(qual, "flatMap", f)
        sameType(expr, newText).option {
          replace(expr).withText(newText).highlightFrom(qual)
        }
      case _ => None
    }

  private def sameType(expr: ScExpression, text: String): Boolean = {
    val newExpr = ScalaPsiElementFactory.createExpressionWithContextFromText(text, expr.getContext, expr)
    expr.`type`().exists { oldType =>
      newExpr.`type`().exists { newType =>
        oldType.equiv(newType)
      }
    }
  }
}
