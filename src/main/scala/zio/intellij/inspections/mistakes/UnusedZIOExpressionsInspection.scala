package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{LocalInspectionTool, LocalQuickFix, ProblemHighlightType, ProblemsHolder}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import zio.intellij.inspections.zioLike

class UnusedZIOExpressionsInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case expr: ScExpression =>
      (expr, expr.nextSiblingNotWhitespace) match {
        case (zioLike(_), Some(zioLike(_))) if !excluded(expr.parent) =>
          Some(
            holder.getManager.createProblemDescriptor(
              expr,
              UnusedZIOExpressionsInspection.message,
              isOnTheFly,
              Array.empty[LocalQuickFix],
              ProblemHighlightType.LIKE_UNUSED_SYMBOL
            )
          )
        case _ => None
      }
    case _ => None
  }

  private def excluded(elem: Option[PsiElement]) =
    elem match {
      case Some(_: ScPrefixExpr) => true
      case _                     => false
    }
}

object UnusedZIOExpressionsInspection {
  val message = "This expression is unused. Did you mean to compose it with another effect?"
}
