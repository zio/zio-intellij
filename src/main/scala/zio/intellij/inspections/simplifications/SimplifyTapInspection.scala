package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScInfixExpr, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import zio.intellij.inspections.ZInspection.simplifyFunctionCall
import zio.intellij.inspections.zioMethods._
import zio.intellij.inspections.{ZInspection, _}

class SimplifyTapInspection
    extends ZInspection(
      FlatMapSimplificationType,
      FlatMapErrorSimplificationType,
      CatchAllToFailSimplificationType,
      TapBothSimplificationType
    )

sealed abstract class BaseRefactoringType(invocation: Qualified, replaceWith: String) extends SimplificationType {
  override def hint: String = s"Replace with .$replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, param: ScParameter, body: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"$replaceWith(${simplifyFunctionCall(param, body)})"))
        .highlightFrom(qual)

    expr match {
      case qual invocation lambda(Seq(param), Some(body) /* once told me */ ) =>
        body match {
          // .flatMap*(a => expr(e).as(a))
          case ref `.as` ScReferenceExpression(a) if a == param => Some(replacement(qual, param, ref))
          // .catchAll(e => expr(e) *> ZIO.fail(e))
          case ScInfixExpr(ref, _, `ZIO.fail`(_)) => Some(replacement(qual, param, ref))
          case _                                  => None
        }
      case _ => None
    }
  }
}

object FlatMapSimplificationType        extends BaseRefactoringType(`.flatMap`, "tap")
object FlatMapErrorSimplificationType   extends BaseRefactoringType(`.flatMapError`, "tapError")
object CatchAllToFailSimplificationType extends BaseRefactoringType(`.catchAll`, "tapError")

object TapBothSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .tapBoth"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, tapError: ScExpression, tap: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"tapBoth(${tapError.getText}, ${tap.getText})"))
        .highlightFrom(qual)

    expr match {
      case qual `.tap` tap `.tapError` tapError => Some(replacement(qual, tapError, tap))
      case qual `.tapError` tapError `.tap` tap => Some(replacement(qual, tapError, tap))
      case _                                    => None
    }
  }

}
