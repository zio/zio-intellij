package org.jetbrains.plugins.scala.codeInspection

import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

// dirty hack to make expressionResultIsNotUsed public
// todo use original method directly when (if) it's a public API
object codeInspectionHack {

  def expressionResultIsNotUsed(expression: ScExpression): Boolean =
    org.jetbrains.plugins.scala.codeInspection.expressionResultIsNotUsed(expression)

}
