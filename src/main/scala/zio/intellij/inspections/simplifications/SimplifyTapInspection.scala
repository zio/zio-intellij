package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{ Simplification, SimplificationType, invocationText }
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ ScExpression, ScInfixExpr }
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import zio.intellij.inspections.zioMethods._
import zio.intellij.inspections.{ ZInspection, _ }

class SimplifyTapInspection extends ZInspection(CatchAllToFailSimplificationType)

object CatchAllToFailSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .tapError"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, param: ScParameter, body: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"tapError(${param.getText} => ${body.getText}"))
        .highlightFrom(qual)

    expr match {
      case qual `.catchAll` lambdaExtractor(ex, body) => Some(replacement(qual, ex, body))
      case _                                          => None
    }
  }
}

object lambdaExtractor {

  def unapply(sc: ScExpression): Option[(ScParameter, ScExpression)] = sc match {
    case lambda(Seq(x), Some(body)) =>
      body match {
        case ScInfixExpr(ref, _, `ZIO.fail`(_)) => Some((x, ref)) // ex => logger.log(ex) *> ZIO.fail(ex)
        case _                                  => None
      }
    case _ => None
  }
}
