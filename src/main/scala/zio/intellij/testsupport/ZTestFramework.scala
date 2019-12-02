package zio.intellij.testsupport

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestFramework

final class ZTestFramework extends AbstractTestFramework {
  override def getSuitePaths: Seq[String]      = ZSuitePaths
  override def getMarkerClassFQName: String    = getSuitePaths.head
  override def getDefaultSuperClass: String    = getSuitePaths.head
  override def getTestFileTemplateName: String = "ZIO Test Suite"
  override def getMnemonic: Char               = 'z'
  override def getName: String                 = "ZIO Test"

  override protected def getLibraryDependencies(scalaVersion: Option[String]): Seq[String] =
    Seq("\"dev.zio\" %% \"zio-test\" % \"latest.integration\" % \"test\"")

  override protected def getLibraryResolvers(scalaVersion: Option[String]): Seq[String] = Seq.empty

  override protected def getAdditionalBuildCommands(scalaVersion: Option[String]): Seq[String] = Seq.empty

  override def isTestMethod(element: PsiElement): Boolean = isTestMethod(element, checkAbstract = false)

  override def isTestMethod(element: PsiElement, checkAbstract: Boolean): Boolean = element.getText == "testM"
}