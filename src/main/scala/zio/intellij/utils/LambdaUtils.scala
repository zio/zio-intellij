package zio.intellij.utils

import org.jetbrains.plugins.scala.codeInsight.intention.expression.ConvertParameterToUnderscoreIntention
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFunctionExpr}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory

object LambdaUtils {

  def lambdaToUnderscore(expr: ScFunctionExpr): ScExpression =
    ConvertParameterToUnderscoreIntention
      .createExpressionToIntroduce(expr, withoutParameterTypes = true)
      .getOrElse(replaceUnusedParamWithUnderscore(expr))

  private def replaceUnusedParamWithUnderscore(e: ScFunctionExpr): ScFunctionExpr =
    e match {
      case ScFunctionExpr(Seq(param), Some(body)) if isUnusedNamedParam(param) =>
        val withWildcard = s"_ => ${body.getText}"
        ScalaPsiElementFactory.safe(_.createElementFromText[ScFunctionExpr](withWildcard, e)(e)).getOrElse(e)
      case _ => e
    }

  private def isUnusedNamedParam(named: ScParameter) = !(isElementUsed(named, isOnTheFly = false) || named.isWildcard)

}
