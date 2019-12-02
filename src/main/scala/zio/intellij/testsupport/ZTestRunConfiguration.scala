package zio.intellij.testsupport

import com.intellij.execution.Executor
import com.intellij.execution.configurations.{JavaCommandLineState, RunProfileState}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiClass
import com.intellij.util.PathUtil
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration

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

    val state = super.getState(executor, env).asInstanceOf[JavaCommandLineState]
    val runnersPath = PathUtil
      .getJarPathForClass(getClass)
      .replace("zio-intellij.jar", "testrunner.jar")

    state.getJavaParameters.getClassPath.add(runnersPath)

    state
  }
}
