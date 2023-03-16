package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import zio.intellij.inspections.{`_ = x`, zioLike}
import zio.intellij.utils.{createForGenerator, fromSameClass}

class DiscardingZIOForBindingInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case scFor @ ScFor(enumerators, _) =>
      enumerators.forBindings.foreach {
        case expr @ `_ = x`(zioLike(body)) if fromSameClass(scFor, body) =>
          holder.registerProblem(
            expr,
            DiscardingZIOForBindingInspection.message,
            ProblemHighlightType.WEAK_WARNING,
            discardingZIOQuickFix(expr, body)
          )
        case _ =>
      }
    case _ =>
  }

  private def discardingZIOQuickFix(toReplace: ScForBinding, generatorBody: ScExpression): LocalQuickFix =
    new AbstractFixOnPsiElement(DiscardingZIOForBindingInspection.hint, toReplace) {
      override protected def doApplyFix(bindingExpr: ScForBinding)(implicit project: Project): Unit =
        createForGenerator("_", generatorBody).foreach(bindingExpr.replace(_))
    }

}

object DiscardingZIOForBindingInspection {

  @Nls
  val message = "Possibly mistaken ignoring of the effect. Perhaps you meant to use `<-` generator?"

  @Nls
  val hint = "Use `<-` generator"

}
