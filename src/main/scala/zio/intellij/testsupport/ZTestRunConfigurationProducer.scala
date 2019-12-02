package zio.intellij.testsupport

import com.intellij.execution.Location
import com.intellij.execution.actions.ConfigurationFromContext
import com.intellij.execution.configurations.RunConfiguration
import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.{PsiDirectory, PsiElement, PsiPackage}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.runner.ScalaApplicationConfigurationProducer
import org.jetbrains.plugins.scala.testingSupport.test.testdata.{ClassTestData, SingleTestData}
import org.jetbrains.plugins.scala.testingSupport.test.{AbstractTestConfigurationProducer, TestConfigurationUtil}

final class ZTestRunConfigurationProducer
    extends AbstractTestConfigurationProducer[ZTestRunConfiguration](new ZTestConfigurationType) {

  override protected def suitePaths: List[String] = ZSuitePaths

  override def isConfigurationByLocation(
    configuration: RunConfiguration,
    location: Location[_ <: PsiElement]
  ): Boolean = {
    val element = location.getPsiElement
    if (element == null) return false
    if (element.isInstanceOf[PsiPackage] || element.isInstanceOf[PsiDirectory]) {
      val result =
        if (!configuration.isInstanceOf[ZTestRunConfiguration]) false
        else TestConfigurationUtil.isPackageConfiguration(element, configuration)
      return result
    }
    val (testClass, testName) = getTestClassWithTestName(location)
    if (testClass == null) return false
    val testClassPath = testClass.qualifiedName
    configuration match {
      case configuration: ZTestRunConfiguration =>
        configuration.testConfigurationData match {
          case testData: SingleTestData => testData.testClassPath == testClassPath && testData.testName == testName
          case classData: ClassTestData => classData.testClassPath == testClassPath && testName == null
          case _                        => false
        }
      case _ => false
    }
  }

  override protected def prepareRunConfiguration(
    runConfiguration: ZTestRunConfiguration,
    location: Location[_ <: PsiElement],
    testClass: ScTypeDefinition,
    testName: String
  ): Unit = {
    super.prepareRunConfiguration(runConfiguration, location, testClass, testName)

    Option(testName).foreach { tn =>
      val testNamePrefixed = s"${testClass.qualifiedName}::$tn"
      runConfiguration.setGeneratedName(testNamePrefixed)
      runConfiguration.setName(testNamePrefixed)
    }
  }

  override def shouldReplace(self: ConfigurationFromContext, other: ConfigurationFromContext): Boolean =
    other.isProducedBy(classOf[ScalaApplicationConfigurationProducer])

  override protected def configurationNameForPackage(packageName: String): String = s"ZIO Tests in $packageName"

  override protected def configurationName(testClass: ScTypeDefinition, testName: String): String =
    StringUtil.getShortName(testClass.qualifiedName)

  override protected def getTestClassWithTestNameImpl(location: Location[_ <: PsiElement]): (ScTypeDefinition, String) =
    location.getPsiElement match {
      case IsZioTestElement(td, tm) =>
        tm match {
          case Some(testName(name)) =>
            (td, name)
          case _ =>
            (td, null)
        }
      case _ => (null, null) // god help me
    }
}
