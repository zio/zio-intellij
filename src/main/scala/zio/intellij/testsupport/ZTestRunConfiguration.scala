package zio.intellij.testsupport

import com.intellij.openapi.project.Project
import com.intellij.openapi.util.InvalidDataException
import com.intellij.psi.PsiClass
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestRunConfiguration
import org.jetbrains.plugins.scala.testingSupport.test.testdata.ClassTestData

final class ZTestRunConfiguration(
  project: Project,
  name: String,
  configurationFactory: ZTestRunConfigurationFactory
) extends AbstractTestRunConfiguration(project, configurationFactory, name, null /* ugh*/ ) {

  override def suitePaths: List[String] = ZSuitePaths

  override def errorMessage: String = "ZIO test is not specified"

  override def allowsSbtUiRun: Boolean = true

  override def testNameKey: String = "-t"

  override def classKey: String = "-s" // dummy parameter, ignored by the specs runner

  override def isInvalidSuite(clazz: PsiClass): Boolean = false

  override def getActionName: String = getName

  override def runnerClassName: String = testConfigurationData match {
    case d: ClassTestData => d.testClassPath
    case o                => throw new InvalidDataException(s"Test configuration kind '${o.getKind}' is not supported.")
  }

  override def reporterClass: String = null
}
