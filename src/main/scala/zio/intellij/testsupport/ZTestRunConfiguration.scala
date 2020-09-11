package zio.intellij.testsupport

import com.intellij.execution.configurations._
import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.testDiscovery.JavaAutoRunManager
import com.intellij.execution.testframework.autotest.{AbstractAutoTestManager, ToggleAutoTestAction}
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties
import com.intellij.execution.testframework.sm.runner.ui.SMTRunnerConsoleView
import com.intellij.execution.testframework.ui.BaseTestsOutputConsoleView
import com.intellij.execution.ui.ConsoleView
import com.intellij.execution.{DefaultExecutionResult, ExecutionException, ExecutionResult, Executor}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.OrderRootType
import com.intellij.openapi.util.InvalidDataException
import com.intellij.psi.PsiClass
import com.intellij.testIntegration.TestFramework
import org.jetbrains.bsp.BspUtil
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.project.ModuleExt
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration.TestFrameworkRunnerInfo
import org.jetbrains.plugins.scala.testingSupport.test._
import org.jetbrains.plugins.scala.testingSupport.test.actions.ScalaRerunFailedTestsAction
import org.jetbrains.plugins.scala.testingSupport.test.sbt._
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{ClassTestData, TestConfigurationData}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class ZTestRunConfiguration(
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

  override protected def runnerInfo: TestFrameworkRunnerInfo =
    TestFrameworkRunnerInfo(
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
    } else
      module.libraries.map(_.getName).exists(_.contains("zio-test-intellij"))

  override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState = {
    val module = getModule
    if (module == null) throw new ExecutionException("Module is not specified")

    new ZioTestCommandLineState(env, module)
  }

  private class ZioTestCommandLineState(env: ExecutionEnvironment, module: Module)
      extends ScalaTestFrameworkCommandLineState(this, env, testConfigurationData, runnerInfo, sbtSupport)(
        project,
        module
      ) {

    override def createJavaParameters(): JavaParameters = {
      val javaParameters = super.createJavaParameters()
      // I'm sorry...
      val params  = javaParameters.getProgramParametersList
      val newList = rebuildList(params.getParameters.asScala.toList)
      params.clearAll()
      params.addAll(newList.asJava)
      javaParameters
    }

    def rebuildList(input: List[String]): List[String] = {
      val mutableList: ListBuffer[String] = ListBuffer.empty[String]
      input
        .sliding(2, 2)
        .toList
        .collect {
          case "-s" :: suite :: _       => mutableList.append("-s", suite)
          case "-testName" :: test :: _ => mutableList.append("-t", test)
        }
      mutableList.toList
    }

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

      createExecutionResult(consoleView, processHandler)
    }

    private def createExecutionResult(
      consoleView: ConsoleView,
      processHandler: ProcessHandler
    ): DefaultExecutionResult = {
      val result         = new DefaultExecutionResult(consoleView, processHandler)
      val restartActions = createRestartActions(consoleView).toSeq.flatten
      result.setRestartActions(restartActions: _*)
      result
    }

    private def createRestartActions(consoleView: ConsoleView) =
      consoleView match {
        case testConsole: BaseTestsOutputConsoleView =>
          val rerunFailedTestsAction = {
            val action = new ScalaRerunFailedTestsAction(testConsole)
            action.init(testConsole.getProperties)
            action.setModelProvider(() => testConsole.asInstanceOf[SMTRunnerConsoleView].getResultsViewer)
            action
          }
          val toggleAutoTestAction = new ToggleAutoTestAction() {
            override def isDelayApplicable: Boolean = false
            override def getAutoTestManager(project: Project): AbstractAutoTestManager =
              JavaAutoRunManager.getInstance(project)
          }
          Some(Seq(rerunFailedTestsAction, toggleAutoTestAction))
        case _ =>
          None
      }
  }

  override protected def validityChecker: SuiteValidityChecker =
    new SuiteValidityCheckerBase {
      override protected def isValidClass(clazz: PsiClass): Boolean           = clazz.is[ScObject]
      override protected def hasSuitableConstructor(clazz: PsiClass): Boolean = true
    }

  override def sbtSupport: SbtTestRunningSupport =
    new SbtTestRunningSupportBase {

      override def commandsBuilder: SbtCommandsBuilder =
        new SbtCommandsBuilderBase {
          override def classKey: Option[String]    = Some("-s")
          override def testNameKey: Option[String] = Some("-t")
        }
    }
}
