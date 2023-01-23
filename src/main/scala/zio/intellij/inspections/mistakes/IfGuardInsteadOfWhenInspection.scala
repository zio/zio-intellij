package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnTwoPsiElements, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGuard}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createElementFromText
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.inspections.mistakes.IfGuardInsteadOfWhenInspection.IfGuardQuickFix
import zio.intellij.inspections._
import zio.intellij.utils.TypeCheckUtils._

class IfGuardInsteadOfWhenInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case element @ `_ <- x`(genExpr)
        if fromZio(genExpr) && IntentionAvailabilityChecker.checkInspection(this, element.getParent) =>
      element.nextSiblingNotWhitespaceComment.foreach {
        case guard @ guard(_) =>
          holder.registerProblem(
            guard,
            IfGuardInsteadOfWhenInspection.problemMessage,
            ProblemHighlightType.WEAK_WARNING,
            new IfGuardQuickFix(genExpr, guard)
          )
        case _ =>
      }
    case _ =>
  }
}

object IfGuardInsteadOfWhenInspection {

  final private class IfGuardQuickFix(generatorExpr: ScExpression, guard: ScGuard)
      extends AbstractFixOnTwoPsiElements(fixMessage, generatorExpr, guard) {

    override protected def doApplyFix(generatorExpr: ScExpression, guard: ScGuard)(implicit project: Project): Unit = {
      val replacement = createElementFromText[ScExpression](
        s"${generatorExpr.getText}.when(${guard.expr.fold("")(_.getText)})",
        generatorExpr
      )(generatorExpr.projectContext)
      generatorExpr.replace(replacement)
      guard.delete()
    }
  }

  @Nls(capitalization = Nls.Capitalization.Sentence)
  val problemMessage =
    "Possibly mistaken use of the if guard statement for the ZIO effect. Perhaps you wanted to use ZIO.when?"

  val fixMessage = "Replace with ZIO.when"
}
