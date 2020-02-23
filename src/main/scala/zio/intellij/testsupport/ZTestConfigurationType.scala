package zio.intellij.testsupport

import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationType}
import com.intellij.openapi.project.DumbAware
import javax.swing.Icon

final class ZTestConfigurationType extends ConfigurationType with DumbAware {
  val confFactory = new ZTestRunConfigurationFactory(this)

  def getConfigurationFactories: Array[ConfigurationFactory] = Array[ConfigurationFactory](confFactory)

  def getDisplayName: String = "ZIO Test"

  def getConfigurationTypeDescription: String = "ZIO testing framework run configuration"

  def getId: String = "ZIOTestRunConfiguration"

  def getIcon: Icon = zio.intellij.ZioIcon
}
