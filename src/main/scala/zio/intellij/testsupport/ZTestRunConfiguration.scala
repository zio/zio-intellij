package zio.intellij.testsupport

import com.intellij.openapi.project.Project
import com.intellij.openapi.util.InvalidDataException
import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.TestRunConfigurationForm.TestKind._
import org.jetbrains.plugins.scala.testingSupport.test.testdata.ClassTestData

final class ZTestRunConfiguration(
  project: Project,
  name: String,
  configurationFactory: ZTestRunConfigurationFactory
) extends AbstractTestRunConfiguration(project, configurationFactory, name, null /* ugh*/ ) {

  override def suitePaths: List[String] = ZSuitePaths

  override def errorMessage: String = "ZIO test is not specified"

  override def allowsSbtUiRun: Boolean = false

  override def testNameKey: String = "-t"

  override def classKey: String = "-s"

  override def isInvalidSuite(clazz: PsiClass): Boolean = false

  override def getActionName: String = getName

  override def reporterClass: String = null

  override def runnerClassName: String =
    testConfigurationData match {
      case d: ClassTestData => d.testClassPath
      case d if d.getKind != CLASS || d.getKind != TEST_NAME =>
        throw new InvalidDataException(s"Test configuration kind '${d.getKind}' is not supported.")
      // todo choose runner based on the sbt configuration
      // "zio.intellij.testsupport.ZTestRunner"
    }
}
