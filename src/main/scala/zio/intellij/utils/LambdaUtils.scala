package zio.intellij.utils

import org.jetbrains.plugins.scala.codeInsight.intention.expression.ConvertParameterToUnderscoreIntention
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScFunctionExpr
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createWildcardNode

object LambdaUtils {

  def lambdaToUnderscore(expr: ScFunctionExpr): String =
    ConvertParameterToUnderscoreIntention
      .createExpressionToIntroduce(expr, true)
      .swap
      .fold(_ => replaceWithUnderscore(expr).getText, _.getText)

  private def replaceWithUnderscore(e: ScFunctionExpr): ScFunctionExpr = {
    val params = e.parameters.filterNot(p => p.isWildcard || p.isImplicitParameter)
    params.collectFirst {
      case named: ScNamedElement if !isElementUsed(named, false) =>
        val wildcard = createWildcardNode(e.getProject).getPsi
        named.nameId.replace(wildcard)
        e
      case _ => e
    }
      .getOrElse(e)
  }

}
