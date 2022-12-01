package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnTwoPsiElements, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGuard, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createElementFromText
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.inspections._
import zio.intellij.inspections.mistakes.IfGuardInsteadOfWhenInspection.createFix
import zio.intellij.utils.TypeCheckUtils._

class IfGuardInsteadOfWhenInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case element @ `_ <- x`(genExpr)
        if fromZio(genExpr) && IntentionAvailabilityChecker.checkInspection(this, element.getParent) =>
      Option(element.getNextSiblingNotWhitespaceComment).collect {
        case guard @ guard(_) =>
          val quickFix = createFix(holder.getManager, isOnTheFly, genExpr, guard)
          holder.registerProblem(quickFix)
      }
    case _ => None
  }
}

object IfGuardInsteadOfWhenInspection {

  final private class IfGuardQuickFix(generatorExpr: ScExpression, guard: ScGuard)
      extends AbstractFixOnTwoPsiElements(fixMessage, generatorExpr, guard) {

    override protected def doApplyFix(generatorExpr: ScExpression, guard: ScGuard)(implicit project: Project): Unit = {
      val replacement = createElementFromText[ScExpression](s"${generatorExpr.getText}.when(${guard.expr.fold("")(_.getText)})", generatorExpr)(generatorExpr.projectContext)
      generatorExpr.replace(replacement)
      guard.delete()
    }
  }

  private def createFix(
    manager: InspectionManager,
    isOnTheFly: Boolean,
    genExpr: ScExpression,
    guard: ScGuard
  ): ProblemDescriptor =
    manager.createProblemDescriptor(
      guard,
      problemMessage,
      isOnTheFly,
      Array[LocalQuickFix](new IfGuardQuickFix(genExpr, guard)),
      ProblemHighlightType.WEAK_WARNING
    )

  @Nls(capitalization = Nls.Capitalization.Sentence)
  val problemMessage =
    "Possibly mistaken use of the if guard statement for the ZIO effect. Perhaps you wanted to use ZIO.when?"

  val fixMessage = "Replace with ZIO.when"
}
