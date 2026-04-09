package zio.intellij.testsupport.bsp

import com.intellij.execution.configurations.RunConfiguration
import org.jetbrains.bsp.project.test.environment.{BspEnvironmentRunnerExtension, ExecutionEnvironmentType}
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{AllInPackageTestData, ClassTestData}
import zio.intellij.testsupport.ZTestRunConfiguration

import scala.jdk.CollectionConverters._

private final class BspEnvironmentZioTestRunnerExtension extends BspEnvironmentRunnerExtension {
  override def runConfigurationSupported(config: RunConfiguration): Boolean =
    config.isInstanceOf[ZTestRunConfiguration]

  override def environmentType: ExecutionEnvironmentType = ExecutionEnvironmentType.TEST

  override def classes(config: RunConfiguration): Option[List[String]] =
    config match {
      case zioTestConfig: ZTestRunConfiguration =>
        zioTestConfig.testConfigurationData match {
          case data: AllInPackageTestData => Some(data.classBuf.asScala.toList)
          case data: ClassTestData        => Some(List(data.testClassPath))
          case _                          => None
        }
      case _ => None
    }
}
