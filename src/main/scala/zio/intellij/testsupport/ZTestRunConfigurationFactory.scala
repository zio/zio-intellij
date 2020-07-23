package zio.intellij.testsupport

import com.intellij.execution.configurations.RunConfiguration
import com.intellij.openapi.project.Project
import javax.swing.Icon
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfigurationFactory

final class ZTestRunConfigurationFactory(configurationType: ZTestConfigurationType)
    extends AbstractTestRunConfigurationFactory(configurationType) {

  override def getIcon: Icon = zio.intellij.ZioIcon

  override def id: String = "ZIO Test"

  override def createTemplateConfiguration(project: Project): RunConfiguration =
    new ZTestRunConfiguration(project, this, "")
}
