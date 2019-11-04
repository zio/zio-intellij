package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import zio.intellij.inspections._

class SimplifyWhenInspection extends ZInspection(WhenSimplificationType)

object WhenSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .when"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(ifStmt: ScExpression, arg: ScReferenceExpression, cond: ScExpression) =
      replace(ifStmt).withText(s"${arg.getText}.when(${cond.getText})")

    expr match {
      case cond @ IfStmt(condition, tb @ zioRef(), `ZIO.unit`()) => Some(replacement(cond, tb, condition))
      case _                                                     => None
    }
  }
}
