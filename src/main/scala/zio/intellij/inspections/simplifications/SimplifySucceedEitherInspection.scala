package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.utils.StringUtils._
import zio.intellij.utils.types.ZioType
import zio.intellij.utils.types.ZioTypes.{UIO, URIO}

class SimplifySucceedEitherInspection extends ZInspection(LeftSimplificationType, RightSimplificationType)

sealed abstract class EitherSimplificationType(extractor: TypeReference, zioMethodName: String)
    extends SimplificationType {

  override def hint: String = s"Replace with ZIO.$zioMethodName"

  private def replacement(zioType: ZioType, zioExpr: ScExpression, eitherArg: ScExpression): Simplification =
    replace(zioExpr)
      .withText(s"${zioType.name}.$zioMethodName${eitherArg.getWrappedText}")
      .highlightAll

  private def getArg(eitherExpr: ScExpression): Option[ScExpression] =
    eitherExpr match {
      case MethodRepr(_, _, _, Seq(arg)) => Some(arg)
      case _                             => None
    }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case `ZIO.succeed`(zioType, extractor(eitherExpr)) => getArg(eitherExpr).map(replacement(zioType, expr, _))
      case `ZIO.apply`(zioType @ (UIO | URIO), extractor(eitherExpr)) =>
        getArg(eitherExpr).map(replacement(zioType, expr, _))
      case _ => None
    }
}

object LeftSimplificationType extends EitherSimplificationType(extractor = scalaLeft, zioMethodName = "left")

object RightSimplificationType extends EitherSimplificationType(extractor = scalaRight, zioMethodName = "right")
