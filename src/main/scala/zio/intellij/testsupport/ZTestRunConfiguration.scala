package zio.intellij.testsupport.test

import com.intellij.execution.configurations.{ConfigurationFactory, JavaCommandLineState, JavaParameters, RunProfileState}
import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties
import com.intellij.execution.ui.ConsoleView
import com.intellij.execution.{DefaultExecutionResult, ExecutionResult, Executor}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.OrderRootType
import com.intellij.openapi.util.InvalidDataException
import com.intellij.testIntegration.TestFramework
import org.jetbrains.bsp.BspUtil
import org.jetbrains.plugins.scala.project.ModuleExt
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration.TestFrameworkRunnerInfo
import org.jetbrains.plugins.scala.testingSupport.test.sbt.SbtTestRunningSupport
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{ClassTestData, TestConfigurationData}
import org.jetbrains.plugins.scala.testingSupport.test.{AbstractTestRunConfiguration, SuiteValidityChecker, TestKind}
import zio.intellij.testsupport.ZSuitePaths

class ZTestRunConfiguration2(
  project: Project,
  configurationFactory: ConfigurationFactory,
  name: String
) extends AbstractTestRunConfiguration(
      project,
      configurationFactory,
      name
    ) { self =>

  override val suitePaths: List[String] = ZSuitePaths

  override val testFramework: TestFramework = TestFramework.EXTENSION_NAME.findExtension(classOf[ZTestFramework])

  override val configurationProducer: ZTestRunConfigurationProducer = ZTestRunConfigurationProducer.instance

  private val ZTestRunnerName = "zio.intellij.testsupport.ZTestRunner"

  override protected val runnerInfo: TestFrameworkRunnerInfo = TestFrameworkRunnerInfo(
    if (Option(getModule).exists(hasTestRunner)) ZTestRunnerName
    else fromTestConfiguration(testConfigurationData)
  )
  override def getActionName: String = getName

  private def fromTestConfiguration(data: TestConfigurationData) =
    data match {
      case d: ClassTestData => d.testClassPath
      case d if d.getKind != TestKind.CLAZZ || d.getKind != TestKind.TEST_NAME =>
        throw new InvalidDataException(s"Test configuration kind '${d.getKind}' is not supported.")
    }

  private[testsupport] def shouldCreateTestConsole: Boolean =
    runnerInfo.runnerClass == ZTestRunnerName

  private def hasTestRunner(module: Module): Boolean =
    if (BspUtil.isBspModule(module)) {
      // todo there must be a better way!
      val allFiles = module.libraries
        .flatMap(_.getUrls(OrderRootType.CLASSES))
        .toSet

      allFiles.exists(_.contains("zio-test-intellij"))
    } else {
      module.libraries.map(_.getName).exists(_.contains("zio-test-intellij"))
    }

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
        res
      }
    }
  }

  override protected def validityChecker = null

  override def sbtSupport = null
}
