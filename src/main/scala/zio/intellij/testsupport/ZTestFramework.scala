package zio.intellij.testsupport

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.ResolvesTo
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.types.ScalaType
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestFramework
import zio.intellij.testsupport.ZTestFramework.testMethodTypes

final class ZTestFramework extends AbstractTestFramework {
  override def getMarkerClassFQName: String = ZSuitePaths.head
  override def getDefaultSuperClass: String = ZSuitePaths.head
  override def testFileTemplateName: String = "ZIO Test Suite"
  override def getName: String              = "ZIO Test"

  override def isTestMethod(element: PsiElement): Boolean = isTestMethod(element, checkAbstract = false)

  override def isTestMethod(element: PsiElement, checkAbstract: Boolean): Boolean =
    element match {
      case sc: ScReferenceExpression => resolvesToTestMethod(sc)
      case _                         => false
    }

  override def baseSuitePaths: Seq[String] = ZSuitePaths

  private def resolvesToTestMethod(sc: ScReferenceExpression): Boolean =
    sc match {
      case ResolvesTo(f: ScFunctionDefinition) =>
        f.returnType match {
          case Right(returnType) =>
            val ret = ScalaType.expandAliases(returnType).getOrElse(returnType)
            testMethodTypes.get(sc.refName).contains(ret.canonicalText)
          case _ => false
        }
      case _ => false
    }
}

object ZTestFramework {
  private[ZTestFramework] val testMethodTypes = Map(
    "suite" -> "_root_.zio.test.Spec[R, E, T]",
    "testM" -> "_root_.zio.test.Spec[R, _root_.zio.test.TestFailure[E], _root_.zio.test.TestSuccess]",
    "test"  -> "_root_.zio.test.Spec[Any, _root_.zio.test.TestFailure[Nothing], _root_.zio.test.TestSuccess]"
  )
}
