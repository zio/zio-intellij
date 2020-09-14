package zio.intellij.utils

import org.jetbrains.plugins.scala.codeInspection.booleans.DoubleNegationUtil
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScInfixExpr, ScParenthesisedExpr, ScPrefixExpr}

import scala.annotation.tailrec

// Taken from Scala Plugin since it's private
// https://github.com/JetBrains/intellij-scala/blob/c5c15cda65abf4ef1b35249945d8d0440bcdfb78/scala/scala-impl/src/org/jetbrains/plugins/scala/codeInspection/booleans/DoubleNegationInspection.scala#L73
object NegationUtils {
  def removeDoubleNegation(expr: ScExpression): ScExpression =
    if (DoubleNegationUtil.hasDoubleNegation(expr)) {
      DoubleNegationUtil.removeDoubleNegation(expr)
    } else expr

  @tailrec
  private def stripParentheses(expr: ScExpression): ScExpression = expr match {
    case ScParenthesisedExpr(inner) => stripParentheses(inner)
    case expr: ScExpression         => expr
  }

  def hasNegation(expr: ScExpression): Boolean = {
    val withoutParentheses = stripParentheses(expr)
    withoutParentheses match {
      case ScPrefixExpr(operation, _)   => operation.refName == "!"
      case ScInfixExpr(_, operation, _) => operation.refName == "!="
      case _                            => false
    }
  }

  def invertedNegationText(expr: ScExpression): String = {
    val withoutParentheses = stripParentheses(expr)
    withoutParentheses match {
      case ScPrefixExpr(_, operand)    => operand.getText
      case ScInfixExpr(left, _, right) => left.getText + "==" + right.getText
    }
  }
}
