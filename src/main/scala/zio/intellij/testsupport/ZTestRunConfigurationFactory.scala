package zio.intellij.testsupport

import com.intellij.execution.configurations.RunConfiguration
import com.intellij.openapi.project.Project
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfigurationFactory

final class ZTestRunConfigurationFactory(configurationType: ZTestConfigurationType)
  extends AbstractTestRunConfigurationFactory(configurationType) {

  override def createTemplateConfiguration(project: Project): RunConfiguration =
      new ZTestRunConfiguration(project, "", this)
}
