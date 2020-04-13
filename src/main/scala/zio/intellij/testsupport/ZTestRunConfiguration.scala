package zio.intellij.testsupport

import com.intellij.execution.configurations.{ JavaCommandLineState, JavaParameters, RunProfileState, RunnerSettings }
import com.intellij.execution.runners.{ ExecutionEnvironment, ProgramRunner }
import com.intellij.execution.testDiscovery.JavaAutoRunManager
import com.intellij.execution.testframework.TestFrameworkRunningModel
import com.intellij.execution.testframework.autotest.{ AbstractAutoTestManager, ToggleAutoTestAction }
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView
import com.intellij.execution.testframework.ui.BaseTestsOutputConsoleView
import com.intellij.execution.{ DefaultExecutionResult, ExecutionResult, Executor }
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{ Getter, InvalidDataException }
import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.actions.AbstractTestRerunFailedTestsAction
import org.jetbrains.plugins.scala.testingSupport.test.testdata.ClassTestData
import zio.intellij.testsupport.internal.ZTestRunnerConsoleProperties

final class ZTestRunConfiguration(
  project: Project,
  name: String,
  configurationFactory: ZTestRunConfigurationFactory
) extends AbstractTestRunConfiguration(project, configurationFactory, name, null /* ugh*/ ) { self =>

  override def suitePaths: List[String] = zio.intellij.testsupport.ZSuitePaths

  override def errorMessage: String = "ZIO test is not specified"

  override def allowsSbtUiRun: Boolean = true

  override def testNameKey: String = "-t"

  override def classKey: String = "-s" // dummy parameter, ignored by the specs runner

  override def isInvalidSuite(clazz: PsiClass): Boolean = false

  override def getActionName: String = getName

  override def reporterClass: String = null

  override def runnerClassName: String = testConfigurationData match {
    case d: ClassTestData => d.testClassPath
    case o                => throw new InvalidDataException(s"Test configuration kind '${o.getKind}' is not supported.")
  }

  override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState = {
    val oldState = super.getState(executor, env).asInstanceOf[JavaCommandLineState]

    new JavaCommandLineState(env) {
      override def createJavaParameters(): JavaParameters = oldState.getJavaParameters

      override def ansiColoringEnabled(): Boolean = false

      override def execute(executor: Executor, runner: ProgramRunner[_ <: RunnerSettings]): ExecutionResult = {
        val processHandler    = startProcess()
        val consoleProperties = new ZTestRunnerConsoleProperties(self, executor)

        val consoleView =
          SMTestRunnerConnectionUtil.createAndAttachConsole("ZIO Test", processHandler, consoleProperties)

        val res = new DefaultExecutionResult(
          consoleView,
          processHandler,
          createActions(consoleView, processHandler, executor): _*
        )

        registerRerunAction(consoleView, res)

        res
      }

      private def registerRerunAction(consoleView: BaseTestsOutputConsoleView, res: DefaultExecutionResult): Unit =
        consoleView match {
          case testConsole: BaseTestsOutputConsoleView =>
            val rerunFailedTestsAction = new AbstractTestRerunFailedTestsAction(testConsole)
            rerunFailedTestsAction.init(testConsole.getProperties)
            rerunFailedTestsAction.setModelProvider(new Getter[TestFrameworkRunningModel] {
              override def get: TestFrameworkRunningModel =
                testConsole.asInstanceOf[SMTRunnerConsoleView].getResultsViewer
            })
            res.setRestartActions(
              rerunFailedTestsAction,
              new ToggleAutoTestAction() {
                override def isDelayApplicable: Boolean = false

                override def getAutoTestManager(project: Project): AbstractAutoTestManager =
                  JavaAutoRunManager.getInstance(project)
              }
            )
          case _ =>
        }
    }
  }

}
