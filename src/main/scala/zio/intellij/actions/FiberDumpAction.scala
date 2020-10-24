package zio.intellij.actions

import com.intellij.debugger.DebuggerManagerEx
import com.intellij.debugger.engine.JavaValue
import com.intellij.execution.filters.{ExceptionFilters, TextConsoleBuilderFactory}
import com.intellij.execution.ui.RunnerLayoutUi
import com.intellij.execution.ui.layout.impl.RunnerContentUi
import com.intellij.notification.NotificationGroup
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, DefaultActionGroup}
import com.intellij.openapi.application.{ApplicationManager, ModalityState}
import com.intellij.openapi.project.{DumbAwareAction, Project}
import com.intellij.openapi.ui.MessageType
import com.intellij.openapi.util.Disposer
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.util.text.DateFormatUtil
import com.intellij.xdebugger.evaluation.EvaluationMode
import com.intellij.xdebugger.frame.XValue
import com.intellij.xdebugger.impl.breakpoints.XExpressionImpl
import com.intellij.xdebugger.impl.ui.tree.nodes.XEvaluationCallbackBase
import com.sun.jdi._
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.project.{ModuleExt, ProjectExt, ScalaLanguageLevel}
import zio.intellij.ui.FiberDumpPanel
import zio.intellij.utils.jdi._
import zio.intellij.utils.jdi.fiber._
import zio.intellij.utils.jdi.fiber.model.FiberInfo
import zio.intellij.utils.{ModuleSyntax, TraverseAtHome, Version}

import scala.annotation.tailrec

final class FiberDumpAction extends DumbAwareAction with AnAction.TransparentUpdate {

  override def actionPerformed(event: AnActionEvent): Unit = {

    val project = event.getProject
    if (project != null) {
      val sourceModules = project.modulesWithScala.filter(_.isSourceModule).toList

      val zioVersion = sourceModules
        .traverse(_.zioVersion)
        .flatMap(_.headOption)

      implicit val scalaLanguageLevel: ScalaLanguageLevel =
        sourceModules
          .flatMap(_.scalaLanguageLevel)
          .headOption
          .getOrElse(ScalaLanguageLevel.getDefault)

      zioVersion.fold(notifyAboutAbsentZioDependency(project)) { implicit version =>
        if (version >= Version.ZIO.RC21)
          notifyAboutNotImplementedFunctionalityForRecentZIOVersions(project)
        else if (version < Version.ZIO.RC18)
          notifyAboutNotImplementedFunctionalityForOldZIOVersions(project)
        else
          invokeDumpAllFibers(project)
      }
    }
  }

  override def update(event: AnActionEvent): Unit = {
    val presentation = event.getPresentation
    val project      = event.getProject
    if (project == null)
      presentation.setEnabled(false)
    else {
      val debuggerSession = DebuggerManagerEx.getInstanceEx(project).getContext.getDebuggerSession
      presentation.setEnabled(debuggerSession != null && debuggerSession.isAttached)
    }
  }

  private def notifyAboutAbsentZioDependency(project: Project): Unit =
    warningNotification(project, "ZIO dependency not found")

  private def notifyAboutNotImplementedFunctionalityForRecentZIOVersions(project: Project): Unit =
    warningNotification(
      project,
      "Dump Fibers functionality is not yet implemented for ZIO version 1.0.0-RC21 and above " +
        "due to changes in fiber tracking"
    )

  private def notifyAboutNotImplementedFunctionalityForOldZIOVersions(project: Project): Unit =
    warningNotification(
      project,
      "Dump Fibers functionality is not implemented for ZIO versions below 1.0.0-RC18 " +
        "due to the fact that the Fiber.dumpAll was added in RC18"
    )

  private def warningNotification(project: Project, message: String): Unit =
    FiberDumpAction.NOTIFICATION_GROUP
      .createNotification(message, MessageType.WARNING)
      .notify(project)

  private def invokeDumpAllFibers(
    project: Project
  )(implicit zioVersion: Version, languageLevel: ScalaLanguageLevel): Unit = {
    val debuggerContext = DebuggerManagerEx.getInstanceEx(project).getContext
    val session         = debuggerContext.getDebuggerSession
    if (session != null && session.isAttached) {
      val process = debuggerContext.getDebugProcess

      val evaluator = process.getXdebugProcess.getEvaluator
      val fiberDumpAllExpr = new XExpressionImpl(
        "new zio.BootstrapRuntime {}.unsafeRun(zio.Fiber.dumpAll).toArray",
        ScalaLanguage.INSTANCE,
        null,
        EvaluationMode.EXPRESSION
      )

      process.getManagerThread.invoke { () =>
        val vmProxy = process.getVirtualMachineProxy
        evaluator.evaluate(
          fiberDumpAllExpr,
          new XEvaluationCallbackBase {
            override def evaluated(result: XValue): Unit = {
              vmProxy.suspend()
              try {
                val dump = buildFiberDump(result)
                ApplicationManager.getApplication.invokeLater(
                  () => {
                    val xSession = session.getXDebugSession
                    if (xSession != null)
                      FiberDumpAction.addFiberDump(project, dump, xSession.getUI, session.getSearchScope)
                  },
                  ModalityState.NON_MODAL
                )
              } finally vmProxy.resume()
            }

            override def errorOccurred(errorMessage: String): Unit =
              FiberDumpAction.NOTIFICATION_GROUP
                .createNotification(
                  s"Error during evaluation of Fiber.dumpAll: $errorMessage",
                  MessageType.ERROR
                )
                .notify(project)
          },
          null
        )
      }
    }
  }

  private def buildFiberDump(
    xValue: XValue
  )(implicit zioVersion: Version, languageLevel: ScalaLanguageLevel): List[FiberInfo] = {
    @tailrec
    def inner(fiberDumps: List[Value], acc: List[FiberInfo]): List[FiberInfo] =
      if (fiberDumps.isEmpty) acc
      else {
        val rawResults           = fiberDumps.flatMap(convertFiberInfoWithChildren(_))
        val currentLevelDump     = rawResults.map(_.info)
        val currentLevelChildren = rawResults.flatMap(_.children)
        inner(currentLevelChildren, acc ++ currentLevelDump)
      }

    xValue match {
      case javaValue: JavaValue =>
        val valueDescriptor = javaValue.getDescriptor
        if (valueDescriptor == null) Nil
        else {
          val fullValueDescriptor = valueDescriptor.getFullValueDescriptor
          if (fullValueDescriptor == null) Nil
          else {
            val dumpValues = convertScalaSeq(fullValueDescriptor.calcValue(javaValue.getEvaluationContext))
            inner(dumpValues, Nil)
          }
        }
      case _ => Nil
    }
  }

}

object FiberDumpAction {

  val NOTIFICATION_GROUP: NotificationGroup = NotificationGroup.balloonGroup("Fiber Dump Notifications")

  def addFiberDump(
    project: Project,
    fibers: List[FiberInfo],
    ui: RunnerLayoutUi,
    searchScope: GlobalSearchScope
  ): Unit = {
    val consoleView = TextConsoleBuilderFactory
      .getInstance()
      .createBuilder(project)
      .filters(ExceptionFilters.getFilters(searchScope))
      .getConsole
    consoleView.allowHeavyFilters()
    val toolbarActions = new DefaultActionGroup()
    val panel          = new FiberDumpPanel(project, consoleView, toolbarActions, fibers)

    val id      = s"Fiber Dump ${DateFormatUtil.formatTimeWithSeconds(System.currentTimeMillis)}"
    val content = ui.createContent(id, panel, id, null, null)
    content.putUserData(RunnerContentUi.LIGHTWEIGHT_CONTENT_MARKER, java.lang.Boolean.TRUE)
    content.setCloseable(true)
    content.setDescription("Fiber Dump")
    ui.addContent(content)
    ui.selectAndFocus(content, true, true)

    Disposer.register(content, consoleView)
    ui.selectAndFocus(content, true, false)
  }

}
