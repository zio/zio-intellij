package zio.intellij.testsupport

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestFramework
import zio.intellij.testsupport.ZTestFramework.testMethods

final class ZTestFramework extends AbstractTestFramework {
  override def getMarkerClassFQName: String = ZSuitePaths.head
  override def getDefaultSuperClass: String = ZSuitePaths.head
  override def testFileTemplateName: String = "ZIO Test Suite"
  override def getName: String              = "ZIO Test"

  override def isTestMethod(element: PsiElement): Boolean = isTestMethod(element, checkAbstract = false)

  override def isTestMethod(element: PsiElement, checkAbstract: Boolean): Boolean =
    element match {
      case sc: ScReferenceExpression => testMethods.contains(sc.refName)
      case _                         => false
    }

  override def baseSuitePaths: Seq[String] = ZSuitePaths
}

object ZTestFramework {
  private[ZTestFramework] val testMethods = Set("test", "testM", "suite")
}
