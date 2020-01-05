package zio.intellij.testsupport

import com.intellij.execution.Executor
import com.intellij.execution.configurations.{JavaCommandLineState, RunProfileState}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiClass
import com.intellij.util.PathUtil
import org.jetbrains.plugins.scala.project._
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration
import scalariform.ScalaVersion

final class ZTestRunConfiguration(
  project: Project,
  name: String,
  configurationFactory: ZTestRunConfigurationFactory
) extends AbstractTestRunConfiguration(project, configurationFactory, name, null /* ugh*/ ) {

  override def suitePaths: List[String] = ZSuitePaths

  override def errorMessage: String = "ZIO test is not specified"

  override def allowsSbtUiRun: Boolean = true

  override protected def sbtTestNameKey: String = " -- -t "

  override def isInvalidSuite(clazz: PsiClass): Boolean = false

  override def getActionName: String = getName

  override def runnerClassName: String = "zio.intellij.testsupport.runner.ZTestRunnerHost"

  override def reporterClass: String = null

  override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState = {
    // .·´¯`(>▂<)´¯`·.
    val scalaVersion = (for {
      module   <- Option(getModule)
      scalaSdk <- module.scalaSdk
      compiler <- scalaSdk.compilerVersion
      version  <- ScalaVersion.parse(compiler)
    } yield version).fold("2.12")(ver => s"${ver.major}.${ver.minor}")

    val state = super.getState(executor, env).asInstanceOf[JavaCommandLineState]
    val runnersPath = PathUtil
      .getJarPathForClass(getClass)
      .replace("zio-intellij.jar", s"testrunner_$scalaVersion.jar")

    state.getJavaParameters.getClassPath.add(runnersPath)

    state
  }
}
