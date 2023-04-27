package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.{expressionResultIsNotUsed, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScBlockStatement, ScExpression}
import zio.intellij.inspections.suiteMethods.suiteAll
import zio.intellij.inspections.{zioLike, zioSpec}

class UnusedZIOExpressionsInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case expr @ zioLike(_) if expressionResultIsNotUsed(expr) =>
      holder.registerProblem(
        expr,
        UnusedZIOExpressionsInspection.unusedZioExprMessage,
        ProblemHighlightType.LIKE_UNUSED_SYMBOL
      )
    case expr @ zioSpec(_) if expressionResultIsNotUsed(expr) && !withinSuiteAll(expr) =>
      holder.registerProblem(
        expr,
        UnusedZIOExpressionsInspection.unusedZioSpecMessage,
        ProblemHighlightType.LIKE_UNUSED_SYMBOL
      )
    case _ =>
  }

  // suiteAll is a macro and Intellij doesn't know about it (yet?)
  private def withinSuiteAll(expr: ScExpression): Boolean =
    // performance-wise should be ok to traverse all parents, since we only do it for "unused" Specs which should be rare
    expr.parentsInFile.collectFirst {
      case `suiteAll`(_, ScBlock(statements @ _*)) => statements
    }.exists(_.exists(isTheSameOrIncludes(expr)))

  private def isTheSameOrIncludes(expr: ScExpression)(stmt: ScBlockStatement): Boolean = {
    def isTheSame = expr == stmt
    def includes = stmt match {
      case ScBlock(statements @ _*) => statements.exists(isTheSameOrIncludes(expr))
      case _                        => false
    }

    isTheSame || includes
  }

}

object UnusedZIOExpressionsInspection {
  @Nls
  val unusedZioExprMessage = "This expression is unused. Did you mean to compose it with another effect?"

  @Nls
  val unusedZioSpecMessage = "This test is ignored. Did you mean to compose it with another test using `+`?"

}
