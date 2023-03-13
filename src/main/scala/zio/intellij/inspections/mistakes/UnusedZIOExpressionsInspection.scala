package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.codeInspection.codeInspectionHack.expressionResultIsNotUsed
import zio.intellij.inspections.{zioLike, zioSpec}

class UnusedZIOExpressionsInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case expr @ zioLike(_) if expressionResultIsNotUsed(expr) =>
      holder.registerProblem(
        expr,
        UnusedZIOExpressionsInspection.unusedZioExprMessage,
        ProblemHighlightType.LIKE_UNUSED_SYMBOL
      )
    case expr @ zioSpec(_) if expressionResultIsNotUsed(expr) =>
      holder.registerProblem(
        expr,
        UnusedZIOExpressionsInspection.unusedZioSpecMessage,
        ProblemHighlightType.LIKE_UNUSED_SYMBOL
      )
    case _ =>
  }

}

object UnusedZIOExpressionsInspection {
  @Nls
  val unusedZioExprMessage = "This expression is unused. Did you mean to compose it with another effect?"

  @Nls
  val unusedZioSpecMessage = "This test is ignored. Did you mean to compose it with another test using `+`?"

}
