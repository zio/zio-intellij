package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.openapi.project.Project
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScThrow}
import zio.intellij.inspections.`ZIO.fail`

class ZIOFailThrowInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case f @ `ZIO.fail`(_, b: ScThrow) =>
      b.expression match {
        case Some(expr) =>
          holder.registerProblem(
            f,
            ZIOFailThrowInspection.message,
            ProblemHighlightType.WARNING,
            new QuickFix(b, expr)
          )
        case None =>
      }
    case _ =>
  }

  final class QuickFix(
    val toReplace: ScExpression,
    val replaceWith: ScExpression
  ) extends AbstractFixOnPsiElement(s"Remove throw from ZIO.fail", toReplace) {

    override protected def doApplyFix(element: ScExpression)(implicit project: Project): Unit =
      element.replace(replaceWith)
  }

}

object ZIOFailThrowInspection {
  val message = "Mistaken throwing of the exception inside ZIO.fail"
}
