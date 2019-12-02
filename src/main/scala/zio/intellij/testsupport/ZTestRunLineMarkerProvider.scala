package zio.intellij.testsupport

import java.util.function.Function

import com.intellij.execution.TestStateStorage
import com.intellij.execution.lineMarker.{ExecutorAction, RunLineMarkerContributor}
import com.intellij.execution.testframework.TestIconMapper
import com.intellij.execution.testframework.sm.runner.states.TestStateInfo._
import com.intellij.icons.AllIcons.RunConfigurations.TestState
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiClass, PsiElement}
import javax.swing.Icon
import org.jetbrains.plugins.scala.decompiler.scalasig.ScalaSigPrinter
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.testingSupport.test.ScalaTestRunLineMarkerProvider

final class ZTestRunLineMarkerProvider extends ScalaTestRunLineMarkerProvider {

  val TooltipProvider: java.util.function.Function[PsiElement, String] = (_: PsiElement) => "Run Test"
  val TestProtocol                                                     = "java:test"

  override def getInfo(element: PsiElement): RunLineMarkerContributor.Info = {
    def buildInfo(td: ScTypeDefinition, tm: Option[ScReferenceExpression]) =
      buildLineInfo(buildUrl(td, tm), td.getProject, tm.isEmpty)

    element match {
      case IsZioTestElement(td, tm) => buildInfo(td, tm)
      case _                        => null /* SIGH */
    }
  }

  def buildUrl(clazz: PsiClass, expr: Option[ScReferenceExpression]): String = {
    val url = s"$TestProtocol://${clazz.qualifiedName}"

    expr match {
      case Some(testName(name)) => s"$url.${ScalaSigPrinter.quote(name)}"
      case None                 => url
    }
  }

  def buildLineInfo(url: String, project: Project, isClass: Boolean): RunLineMarkerContributor.Info = {
    val icon    = iconFor(url, project, isClass)
    val actions = ExecutorAction.getActions(1)
    new ReplacementInfo(icon, actions, TooltipProvider)
  }

  private def iconFor(url: String, project: Project, isClass: Boolean): Icon = {
    import Magnitude._

    def defaultIcon =
      if (isClass) TestState.Run_run
      else TestState.Run

    val testState     = Option(TestStateStorage.getInstance(project).getState(url))
    val testMagnitude = testState.map(state => TestIconMapper.getMagnitude(state.magnitude))

    testMagnitude.fold(defaultIcon) {
      case ERROR_INDEX | FAILED_INDEX    => TestState.Red2
      case PASSED_INDEX | COMPLETE_INDEX => TestState.Green2
      case _                             => defaultIcon
    }
  }

  private[this] class ReplacementInfo(
    icon: Icon,
    actions: Array[AnAction],
    tooltipProvider: Function[PsiElement, String]
  ) extends RunLineMarkerContributor.Info(icon, actions, tooltipProvider) {
    override def shouldReplace(other: RunLineMarkerContributor.Info): Boolean =
      true
  }
}
