package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{ Simplification, SimplificationType, invocationText }
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ ScExpression, ScInfixExpr, ScReferenceExpression }
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import zio.intellij.inspections.zioMethods._
import zio.intellij.inspections.{ ZInspection, _ }

class SimplifyTapInspection extends ZInspection(FlatMapSimplificationType, CatchAllToFailSimplificationType)

object FlatMapSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .tap"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, param: ScParameter, body: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"tap(${param.getText} => ${body.getText}"))
        .highlightFrom(qual)

    expr match {
      case qual `.flatMap` lambda(Seq(param), Some(body)) =>
        body match {
          case ref `.as` ScReferenceExpression(a) if a == param => Some(replacement(qual, param, ref))
          case _                                                => None
        }
      case _ => None
    }
  }
}

object CatchAllToFailSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .tapError"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, param: ScParameter, body: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"tapError(${param.getText} => ${body.getText}"))
        .highlightFrom(qual)

    expr match {
      case qual `.catchAll` lambda(Seq(ex), Some(body)) => // .catchAll(ex => logger.log(ex) *> ZIO.fail(ex))
        body match {
          case ScInfixExpr(ref, _, `ZIO.fail`(_)) => Some(replacement(qual, ex, ref))
          case _                                  => None
        }
      case _ => None
    }
  }
}
