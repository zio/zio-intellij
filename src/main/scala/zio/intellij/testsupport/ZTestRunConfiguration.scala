package zio.intellij.testsupport

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
import com.intellij.util.PathUtil
import com.intellij.util.ui.UIUtil
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.testingSupport.test.CustomTestRunnerBasedStateProvider.TestFrameworkRunnerInfo
import org.jetbrains.plugins.scala.testingSupport.test._
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{
  AllInPackageTestData,
  ClassTestData,
  TestConfigurationData
}
import zio.intellij.testsupport.ZTestRunConfiguration.{ZTestRunnerName, Zio2TestRunnerName}
import zio.intellij.testsupport.zio1.runner.{TestRunnerResolveService => Zio1TestRunnerResolveService}
import zio.intellij.utils.ZioVersion.ZIO
import zio.intellij.utils._

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

final class Zio1TestRunConfiguration(project: Project, configurationFactory: ConfigurationFactory)
    extends ZTestRunConfiguration(project, configurationFactory, "") {
  override val testFramework: ZTestFramework = TestFramework.EXTENSION_NAME.findExtension(classOf[Zio1TestFramework])
}
final class Zio2TestRunConfiguration(project: Project, configurationFactory: ConfigurationFactory)
    extends ZTestRunConfiguration(project, configurationFactory, "") {
  override val testFramework: ZTestFramework = TestFramework.EXTENSION_NAME.findExtension(classOf[Zio2TestFramework])
}

sealed abstract class ZTestRunConfiguration(project: Project, configurationFactory: ConfigurationFactory, name: String)
    extends AbstractTestRunConfiguration(project, configurationFactory, name) {
  self =>

  override val configurationProducer: ZTestRunConfigurationProducer =
    RunConfigurationProducer.EP_NAME.findExtension(classOf[ZTestRunConfigurationProducer])

  override protected val validityChecker: SuiteValidityChecker = ZTestRunConfiguration.validityChecker

  private lazy val isZio2: Boolean =
    Option(self.getModule).flatMap(_.zioVersion).exists(_ >= ZIO.`2.0.0`)

  private def fromTestConfiguration(data: TestConfigurationData) =
    data match {
      case d: ClassTestData                  => d.testClassPath
      case _: AllInPackageTestData if isZio2 => Zio2TestRunnerName
      case d                                 => throw new InvalidDataException(s"Test configuration kind '${d.getKind}' is not supported.")
    }

  private def runnerInfo =
    TestFrameworkRunnerInfo(
      Option(self.getModule).flatMap { module =>
        Option.when(module.zioVersion.exists(_.requiresTestRunner) && hasZio1TestRunner(module))(ZTestRunnerName)
      }.getOrElse(fromTestConfiguration(testConfigurationData))
    )

  override def runStateProvider: RunStateProvider =
    (env: ExecutionEnvironment, failedTests: Option[Seq[(String, String)]]) => {
      val testRunnerJars = Option(self.getModule).flatMap(resolveTestRunner)

      new ZioTestCommandLineState(env, failedTests, testRunnerJars)
    }

  override def getActionName: String = getName

  private def useIntegratedRunner: Boolean =
    runnerInfo.runnerClass == ZTestRunnerName || isZio2

  private def resolveTestRunner(module: Module): Option[Seq[URI]] =
    if (isZio2) resolveZio2TestRunner()
    else resolveZio1TestRunner(module)

  private def resolveZio1TestRunner(module: Module): Option[Seq[URI]] =
    module.zioVersion zip module.scalaVersion match {
      case Some((zioVersion, scalaVersion)) if zioVersion.requiresTestRunner =>
        Zio1TestRunnerResolveService
          .instance(module.getProject)
          .resolve(zioVersion, scalaVersion, downloadIfMissing = false)
          .toOption
          .map(_.toIndexedSeq)
      case _ => None
    }

  private def resolveZio2TestRunner(): Option[Seq[URI]] = {
    // the runner is only used for "All in package" test runs
    if (testConfigurationData.getKind != TestKind.ALL_IN_PACKAGE) return None

    val libDir    = Paths.get(PathUtil.getJarPathForClass(this.getClass)).getParent
    val runnerJar = libDir.resolve("zio2-test-runner.jar")
    if (Files.exists(runnerJar)) Some(Seq(runnerJar.toUri)) else None
  }

  private def hasZio1TestRunner(module: Module): Boolean =
    module.findLibrary(_.contains("zio-test-intellij")).isDefined ||
      resolveZio1TestRunner(module).isDefined

  class ZioTestCommandLineState(
    env: ExecutionEnvironment,
    failedTests: Option[Seq[(String, String)]],
    testRunnerJars: Option[Seq[URI]]
  ) extends ScalaTestFrameworkCommandLineState(self, env, failedTests, runnerInfo) {

    override def createJavaParameters(): JavaParameters = {
      val javaParameters = super.createJavaParameters()

      testRunnerJars.foreach { urls =>
        javaParameters.getClassPath.addAll(urls.map(Paths.get(_).toFile.toString).asJava)
      }

      val params = javaParameters.getProgramParametersList
      // The Scala plugin may write spec class names to a temp @argfile when there are
      // multiple test classes (e.g. "All in package"). Expand any such references here
      // so that rebuildList receives the full flat list of args.
      val expandedArgs = params.getParameters.asScala.toList.flatMap {
        case arg if arg.startsWith("@") =>
          val path = Paths.get(arg.substring(1))
          if (Files.exists(path)) Files.readAllLines(path, StandardCharsets.UTF_8).asScala.toList
          else List(arg)
        case arg => List(arg)
      }
      val newList = rebuildList(expandedArgs)
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
      if (isZio2) {
        mutableList.appendAll(Seq("-renderer", "intellij", "-summary", "false"))
      }
      mutableList.toList
    }

    override def execute(executor: Executor, runner: ProgramRunner[_]): ExecutionResult = {
      val processHandler = startProcess()

      val consoleView: ConsoleView =
        UIUtil.invokeAndWaitIfNeeded { () =>
          if (useIntegratedRunner) {
            val consoleProperties = new ZTestFrameworkConsoleProperties(self, executor)
            SMTestRunnerConnectionUtil.createAndAttachConsole(
              consoleProperties.getTestFrameworkName,
              processHandler,
              consoleProperties
            )
          } else {
            val console = new ConsoleViewImpl(project, true)
            console.attachToProcess(processHandler)
            console
          }
        }

      // TODO figure out whether we need a dedicated testConsoleView
      createExecutionResult(consoleView, consoleView, processHandler)
    }
  }

}

object ZTestRunConfiguration {
  val ZTestRunnerName    = "zio.intellij.testsupport.ZTestRunner"
  val Zio2TestRunnerName = "zio.intellij.testsupport.zio2.runner.ZTestRunner"

  private val validityChecker =
    new SuiteValidityCheckerBase {
      override protected def isValidClass(clazz: PsiClass): Boolean = clazz.is[ScObject]

      override protected def hasSuitableConstructor(clazz: PsiClass): Boolean = true
    }
}
