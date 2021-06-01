package zio.intellij.testsupport

import java.net.URL
import com.intellij.execution.actions.RunConfigurationProducer
import com.intellij.execution.configurations._
import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.runners.{ExecutionEnvironment, ProgramRunner}
import com.intellij.execution.testframework.sm.SMTestRunnerConnectionUtil
import com.intellij.execution.ui.ConsoleView
import com.intellij.execution.{ExecutionResult, Executor}
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.InvalidDataException
import com.intellij.psi.PsiClass
import com.intellij.testIntegration.TestFramework
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.testingSupport.test.CustomTestRunnerBasedStateProvider.TestFrameworkRunnerInfo
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{ClassTestData, TestConfigurationData}
import org.jetbrains.plugins.scala.testingSupport.test.{SuiteValidityChecker, _}
import zio.intellij.testsupport.ZTestRunConfiguration.ZTestRunnerName
import zio.intellij.testsupport.runner.TestRunnerResolveService
import zio.intellij.utils._

import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

final class ZTestRunConfiguration(project: Project, configurationFactory: ConfigurationFactory, name: String)
    extends AbstractTestRunConfiguration(project, configurationFactory, name) {
  self =>

  override val testFramework: ZTestFramework = TestFramework.EXTENSION_NAME.findExtension(classOf[ZTestFramework])

  override val configurationProducer: ZTestRunConfigurationProducer =
    RunConfigurationProducer.EP_NAME.findExtension(classOf[ZTestRunConfigurationProducer])

  override protected val validityChecker: SuiteValidityChecker = ZTestRunConfiguration.validityChecker

  private def fromTestConfiguration(data: TestConfigurationData) =
    data match {
      case d: ClassTestData => d.testClassPath
      case d if d.getKind != TestKind.CLAZZ || d.getKind != TestKind.TEST_NAME =>
        throw new InvalidDataException(s"Test configuration kind '${d.getKind}' is not supported.")
    }

  private def runnerInfo =
    TestFrameworkRunnerInfo(
      Option(self.getModule).flatMap { module =>
        if (hasTestRunner(module)) Some(ZTestRunnerName)
        else None
      }.getOrElse(fromTestConfiguration(testConfigurationData))
    )

  override def runStateProvider: RunStateProvider =
    (env: ExecutionEnvironment, failedTests: Option[Seq[(String, String)]]) => {
      val testRunnerJars = Option(self.getModule).flatMap(resolveTestRunner)

      new ZioTestCommandLineState(env, failedTests, testRunnerJars)
    }

  override def getActionName: String = getName

  private def useIntegratedRunner: Boolean =
    runnerInfo.runnerClass == ZTestRunnerName

  private def resolveTestRunner(module: Module): Option[Seq[URL]] =
    module.zioVersion zip module.scalaVersion match {
      case Some((zioVersion, scalaVersion)) =>
        TestRunnerResolveService.instance(module.getProject).resolve(zioVersion, scalaVersion, false).toOption
      case _ => None
    }

  private def hasTestRunner(module: Module): Boolean =
    module.findLibrary(_.contains("zio-test-intellij")).isDefined ||
      resolveTestRunner(module).isDefined

  class ZioTestCommandLineState(
    env: ExecutionEnvironment,
    failedTests: Option[Seq[(String, String)]],
    testRunnerJars: Option[Seq[URL]]
  ) extends ScalaTestFrameworkCommandLineState(self, env, failedTests, runnerInfo) {

    override def createJavaParameters(): JavaParameters = {
      val javaParameters = super.createJavaParameters()

      testRunnerJars.foreach { urls =>
        javaParameters.getClassPath.addAll(urls.map(u => Paths.get(u.toURI).toFile.toString).asJava)
      }

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
          case "-s" :: suite :: _       => mutableList.appendAll(Seq("-s", suite))
          case "-testName" :: test :: _ => mutableList.appendAll(Seq("-t", test))
        }
      mutableList.toList
    }

    override def execute(executor: Executor, runner: ProgramRunner[_]): ExecutionResult = {
      val processHandler = startProcess()

      val consoleView: ConsoleView =
        if (useIntegratedRunner) {
          val consoleProperties = new ScalaTestFrameworkConsoleProperties(self, "ZIO Test", executor)
          SMTestRunnerConnectionUtil.createAndAttachConsole("ZIO Test", processHandler, consoleProperties)
        } else {
          val console = new ConsoleViewImpl(project, true)
          console.attachToProcess(processHandler)
          console
        }

      createExecutionResult(consoleView, processHandler)
    }
  }

}

object ZTestRunConfiguration {
  val ZTestRunnerName = "zio.intellij.testsupport.ZTestRunner"

  private val validityChecker =
    new SuiteValidityCheckerBase {
      override protected def isValidClass(clazz: PsiClass): Boolean = clazz.is[ScObject]

      override protected def hasSuitableConstructor(clazz: PsiClass): Boolean = true
    }
}
