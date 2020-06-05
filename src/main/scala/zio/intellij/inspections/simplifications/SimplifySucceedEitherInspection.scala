package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._

class SimplifySucceedEitherInspection extends ZInspection(LeftSimplificationType, RightSimplificationType)

sealed abstract class EitherSimplificationType(extractor: TypeReference, zioMethodName: String)
    extends SimplificationType {
  private val UIOApply = new Apply(Set("zio.UIO"))

  override def hint: String = s"Replace with ZIO.$zioMethodName"

  private def replacement(zioExpr: ScExpression, eitherArg: ScExpression): Simplification =
    replace(zioExpr)
      .withText(s"ZIO.$zioMethodName(${eitherArg.getText}")
      .highlightFrom(zioExpr)

  private def getArg(eitherExpr: ScExpression): Option[ScExpression] = eitherExpr match {
    case MethodRepr(_, _, _, Seq(arg)) => Some(arg)
    case _                             => None
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(extractor(eitherExpr)) => getArg(eitherExpr).map(replacement(expr, _))
      case UIOApply(extractor(eitherExpr))      => getArg(eitherExpr).map(replacement(expr, _))
      case _                                    => None
    }
}

object LeftSimplificationType extends EitherSimplificationType(extractor = scalaLeft, zioMethodName = "left")

object RightSimplificationType extends EitherSimplificationType(extractor = scalaRight, zioMethodName = "right")
