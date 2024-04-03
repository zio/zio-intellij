package zio.intellij.testsupport

import com.intellij.execution.TestStateStorage
import com.intellij.execution.lineMarker.{ExecutorAction, RunLineMarkerContributor}
import com.intellij.execution.testframework.TestIconMapper
import com.intellij.execution.testframework.sm.runner.states.TestStateInfo._
import com.intellij.icons.AllIcons.RunConfigurations.TestState
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiClass, PsiElement}
import org.jetbrains.plugins.scala.decompiler.scalasig.ScalaSigPrinter
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.testingSupport.test.ui.ScalaTestRunLineMarkerProvider

import java.util.function.Function
import javax.swing.Icon

final class ZTestRunLineMarkerProvider extends ScalaTestRunLineMarkerProvider {

  private val TooltipProvider: java.util.function.Function[PsiElement, String] = (_: PsiElement) => "Run Test"

  override def getInfo(element: PsiElement): RunLineMarkerContributor.Info = {
    def buildInfo(td: ScTypeDefinition, tm: Option[ScReferenceExpression]) =
      tm match {
        case Some(method) if method.refName == "suite" =>
          buildLineInfo(buildUrl(tm), td.getProject, isClass = true)
        case Some(method) =>
          buildLineInfo(buildUrl(tm), td.getProject, tm.isEmpty)
        case _ => null
      }

    element match {
      case IsZioTestElement(td, tm) => buildInfo(td, tm)
      case _                        => null /* SIGH */
    }
  }

  private def buildUrl(expr: Option[ScReferenceExpression]): String =
    expr.map { e =>
      val file              = e.getContainingFile
      val document          = file.getViewProvider.getDocument
      val urlWithLineNumber = s"${file.getVirtualFile.getUrl}:${document.getLineNumber(e.getTextOffset) + 1}"
      urlWithLineNumber
    }.orNull

  override def buildLineInfo(url: String, project: Project, isClass: Boolean): RunLineMarkerContributor.Info = {
    val icon    = iconFor(project, isClass, Option(url))
    val actions = ExecutorAction.getActions(1)
    new ReplacementInfo(icon, actions, TooltipProvider)
  }

  private def iconFor(project: Project, isClass: Boolean, url: Option[String]): Icon = {
    import Magnitude._

    def defaultIcon =
      if (isClass) TestState.Run_run
      else TestState.Run

    val testState     = url.flatMap(url => Option(TestStateStorage.getInstance(project).getState(url)))
    val testMagnitude = testState.map(state => TestIconMapper.getMagnitude(state.magnitude))

    testMagnitude.collect {
      case ERROR_INDEX                   => TestState.Red2
      case FAILED_INDEX                  => TestState.Yellow2
      case PASSED_INDEX | COMPLETE_INDEX => TestState.Green2
    }.getOrElse(defaultIcon)
  }

  private[this] class ReplacementInfo(
    icon: Icon,
    actions: Array[AnAction],
    tooltipProvider: Function[PsiElement, String]
  ) extends RunLineMarkerContributor.Info(icon, actions, tooltipProvider) {
    override def shouldReplace(other: RunLineMarkerContributor.Info): Boolean = true
  }
}
