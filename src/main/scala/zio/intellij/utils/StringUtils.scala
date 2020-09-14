package zio.intellij.utils

import org.jetbrains.plugins.scala.extensions.{PsiElementExt, StringExt}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression

object StringUtils {

  implicit class ScExpressionExt(val expr: ScExpression) extends AnyVal {

    // injects only braces if required
    def getBracedText: String = wrap(needParenthesis = false)

    // injects braces or parenthesis depending on the expression content
    def getWrappedText: String = wrap()

    private def wrap(needParenthesis: Boolean = true, needBraces: Boolean = true): String = {
      val exprText = expr.getText
      // stupid hack found in scala plugin
      val bracesRequired = exprText.contains('\n')

      if (bracesRequired)
        exprText.braced(needBraces = needBraces && !expr.startsWithToken(ScalaTokenTypes.tLBRACE))
      else
        exprText.parenthesize(needParenthesis = needParenthesis && !expr.startsWithToken(ScalaTokenTypes.tLPARENTHESIS))
    }
  }

}
