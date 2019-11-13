package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import zio.intellij.inspections._

class SimplifyWhenInspection extends ZInspection(WhenSimplificationType)

object WhenSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .when"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(ifStmt: ScExpression, tb: ScReferenceExpression, arg: ScExpression, cond: ScExpression) = {
      val refText = if (tb == arg) tb.getText else s"${tb.getText}(${arg.getText})"
      replace(ifStmt).withText(s"$refText.when(${cond.getText})")
    }

    expr match {
      case cond @ IfStmt(condition, zioRef(tb, e), `ZIO.unit`(_)) => Some(replacement(cond, tb, e, condition))
      case _                                                      => None
    }
  }
}
