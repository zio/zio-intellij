package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._

class SimplifyWhenInspection extends ZInspection(WhenSimplificationType)

object WhenSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .when"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(ifStmt: ScExpression, body: ScExpression, cond: ScExpression) =
      replace(ifStmt).withText(s"${body.getText}.when(${cond.getText})")

    expr match {
      case ifStmt @ IfStmt(condition, body @ zioRef(_, _), `ZIO.unit`(_)) => Some(replacement(ifStmt, body, condition))
      case _                                                              => None
    }
  }
}
