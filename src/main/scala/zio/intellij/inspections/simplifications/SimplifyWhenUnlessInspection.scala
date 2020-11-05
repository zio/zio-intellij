package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.utils.NegationUtils.{hasNegation, invertedNegationText, removeDoubleNegation}
import zio.intellij.utils.OptionUtils

class SimplifyWhenInspection   extends ZInspection(WhenSimplificationType)
class SimplifyUnlessInspection extends ZInspection(UnlessSimplificationType)

sealed abstract class WhenUnlessSimplificationTypeBase(replacementMethod: String) extends SimplificationType {
  override def hint: String = s"Replace with .$replacementMethod"

  private def replacement(ifStmt: ScExpression, body: ScExpression, conditionText: String): Simplification =
    replace(ifStmt).withText(s"${body.getText}.$replacementMethod($conditionText)")

  protected def replacement(
    ifStmt: ScExpression,
    body: ScExpression,
    condition: ScExpression,
    shouldHaveNegation: Boolean
  ): Option[Simplification] = {
    val conditionWithoutDoubleNegation = removeDoubleNegation(condition)
    if (hasNegation(conditionWithoutDoubleNegation)) {
      OptionUtils.when(shouldHaveNegation) {
        replacement(ifStmt, body, invertedNegationText(conditionWithoutDoubleNegation))
      }
    } else {
      OptionUtils.unless(shouldHaveNegation) {
        replacement(ifStmt, body, conditionWithoutDoubleNegation.getText)
      }
    }
  }

}

object WhenSimplificationType extends WhenUnlessSimplificationTypeBase("when") {
  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case ifStmt @ IfStmt(condition, body @ zioLike(_), `ZIO.unit`(_)) =>
        replacement(ifStmt, body, condition, shouldHaveNegation = false)
      case ifStmt @ IfStmt(condition, `ZIO.unit`(_), body @ zioLike(_)) =>
        replacement(ifStmt, body, condition, shouldHaveNegation = true)
      case _ => None
    }
}

object UnlessSimplificationType extends WhenUnlessSimplificationTypeBase("unless") {
  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case ifStmt @ IfStmt(condition, body @ zioLike(_), `ZIO.unit`(_)) =>
        replacement(ifStmt, body, condition, shouldHaveNegation = true)
      case ifStmt @ IfStmt(condition, `ZIO.unit`(_), body @ zioLike(_)) =>
        replacement(ifStmt, body, condition, shouldHaveNegation = false)
      case _ => None
    }
}
