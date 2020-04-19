package zio.intellij.testsupport

import com.intellij.execution.configurations.{ JavaCommandLineState, JavaParameters, RunProfileState, RunnerSettings }
import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.runners.{ ExecutionEnvironment, ProgramRunner }
import com.intellij.execution.testDiscovery.JavaAutoRunManager
import com.intellij.execution.testframework.TestFrameworkRunningModel
import com.intellij.execution.testframework.autotest.{ AbstractAutoTestManager, ToggleAutoTestAction }
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView
import com.intellij.execution.testframework.ui.BaseTestsOutputConsoleView
import com.intellij.execution.ui.ConsoleView
import com.intellij.execution.{ DefaultExecutionResult, ExecutionResult, Executor }
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.{ Getter, InvalidDataException }
import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.project.ModuleExt
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.TestRunConfigurationForm.TestKind._
import org.jetbrains.plugins.scala.testingSupport.test.actions.AbstractTestRerunFailedTestsAction
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{ ClassTestData, TestConfigurationData }

final class ZTestRunConfiguration(
  project: Project,
  name: String,
  configurationFactory: ZTestRunConfigurationFactory
) extends AbstractTestRunConfiguration(project, configurationFactory, name, null /* ugh*/ ) { self =>

  private val ZTestRunnerName = "zio.intellij.testsupport.ZTestRunner"

  override def suitePaths: List[String] = ZSuitePaths

  override def errorMessage: String = "ZIO test is not specified"

  override def allowsSbtUiRun: Boolean = false

  override def testNameKey: String = "-t"

  override def classKey: String = "-s"

  override def isInvalidSuite(clazz: PsiClass): Boolean = false

  override def getActionName: String = getName

  override def reporterClass: String = null

  override def runnerClassName: String =
    if (Option(getModule).exists(hasTestRunner)) ZTestRunnerName
    else fromTestConfiguration(testConfigurationData)

  private def fromTestConfiguration(data: TestConfigurationData) =
    data match {
      case d: ClassTestData => d.testClassPath
      case d if d.getKind != CLAZZ || d.getKind != TEST_NAME =>
        throw new InvalidDataException(s"Test configuration kind '${d.getKind}' is not supported.")
    }

  private[testsupport] def shouldCreateTestConsole: Boolean =
    runnerClassName == ZTestRunnerName

  private def hasTestRunner(module: Module): Boolean =
    module.libraries.map(_.getName).exists(_.contains("zio-test-intellij"))

  override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState = {
    val oldState = super.getState(executor, env).asInstanceOf[JavaCommandLineState]

    new JavaCommandLineState(env) {
      override def createJavaParameters(): JavaParameters = oldState.getJavaParameters

      override def execute(executor: Executor, runner: ProgramRunner[_]): ExecutionResult = {
        val processHandler = startProcess()

        val consoleView: ConsoleView =
          if (shouldCreateTestConsole) {
            val consoleProperties = new SMTRunnerConsoleProperties(self, "ZIO Test", executor)
            SMTestRunnerConnectionUtil.createAndAttachConsole("ZIO Test", processHandler, consoleProperties)
          } else {
            val console = new ConsoleViewImpl(project, true)
            console.attachToProcess(processHandler)
            console
          }

        val res = new DefaultExecutionResult(
          consoleView,
          processHandler,
          createActions(consoleView, processHandler, executor): _*
        )

        registerRerunAction(consoleView, res)
      }

      private def registerRerunAction(
        consoleView: ConsoleView,
        res: DefaultExecutionResult
      ): DefaultExecutionResult =
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
            res
          case _ => res
        }
    }
  }
}
