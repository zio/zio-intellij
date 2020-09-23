package zio.intellij.inspections.simplifications

import com.intellij.psi.search.LocalSearchScope
import com.intellij.psi.search.searches.ReferencesSearch
import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScBindingPattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScTemplateBody
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods.`.*>`
import zio.intellij.utils.TypeCheckUtils._

class SimplifyChainToForInspection extends ZInspection(ChainToForSimplificationType, ForToChainSimplificationType)

object ChainToForSimplificationType extends SimplificationType {
  override def hint: String = "Convert to for-comprehension"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    (expr, expr.parent) match {
      case (
            x `.*>` y,
            Some(
              _: ScTemplateBody |       // class/trait/etc body
              _: ScBlock |              // { ... }
              _: ScPatternDefinition |  // val foo = ...
              _: ScFunctionDefinition | // def foo = ...
              _: ScParenthesisedExpr    // (...)
            )
          ) =>
        val exprs = collectFromChain(x, y :: Nil)
        val text =
          s"""for {
             |${exprs.init.map(e => s"_ <- ${e.getText}").mkString(System.lineSeparator())}
             |r <- ${exprs.last.getText}
             |} yield r""".stripMargin

        ScalaPsiElementFactory.createExpressionFromText(text, context = expr) match {
          case forExpr: ScFor => Some(replace(expr).withText(forExpr.getText).highlightFrom(expr))
          case _              => None
        }
      case _ => None
    }

  @annotation.tailrec
  def collectFromChain(expr: ScExpression, exprs: List[ScExpression]): List[ScExpression] =
    expr match {
      case x `.*>` y => collectFromChain(x, y :: exprs)
      case x         => x :: exprs
    }

}

object ForToChainSimplificationType extends SimplificationType {
  override def hint: String = "Convert to `*>` chain"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case forExpr: ScFor if forExpr.`type`().exists(fromZioLike) =>
        forExpr.enumerators.flatMap { enumerators =>
          if (enumerators.guards.isEmpty && enumerators.forBindings.isEmpty && enumerators.generators.nonEmpty)
            forExpr.body match {
              case Some(body)
                  if body.textMatches(enumerators.generators.last.pattern) && enumerators.generators.init.forall(
                    patternIsWildcardOrNotUsed(forExpr)
                  ) =>
                val expressions = enumerators.generators.flatMap(_.expr).map(_.getText)
                Some(replace(forExpr).withText(expressions.mkString(" *> ")).highlightFrom(forExpr))
              case _ => None
            }
          else None
        }
      case _ => None
    }

  private def patternIsWildcardOrNotUsed(forExpr: ScFor)(gen: ScGenerator): Boolean =
    gen.pattern match {
      case _: ScWildcardPattern => true
      case p: ScBindingPattern  => ReferencesSearch.search(p, new LocalSearchScope(forExpr)).findFirst() == null
      case _                    => false
    }
}
