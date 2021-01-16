package zio.intellij.testsupport

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.extensions.ResolvesTo
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.ScClassImpl
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestFramework
import zio.intellij.testsupport.ZTestFramework.expandsToTestMethod

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
      case ResolvesTo(f: ScFunctionDefinition) if !excluded(f) =>
        f.returnType match {
          case Right(returnType) if isOfClassFrom(returnType, Array("zio.test._")) =>
            expandsToTestMethod(returnType)
          case _ => false
        }
      case _ => false
    }

  private def excluded(fd: ScFunctionDefinition): Boolean =
    // TODO hack. Figure out a good way to filter
    // (e.g. via the argument (TestAspect) or the return type)
    fd.name == "@@"
}

object ZTestFramework {
  private[ZTestFramework] val testMethodTypes = Set(
    "_root_.zio.test.Spec[R, E, T]",
    "_root_.zio.test.Spec[R, _root_.zio.test.TestFailure[E], _root_.zio.test.TestSuccess]",
    "_root_.zio.test.Spec[Any, _root_.zio.test.TestFailure[Nothing], _root_.zio.test.TestSuccess]",
    "_root_.zio.test.MutableRunnableSpec.SuiteBuilder",
    "_root_.zio.test.MutableRunnableSpec.TestBuilder"
  )

  private def expandsToTestMethod(tpe: ScType) =
    tpe.extractClass.collect {
      case c: ScClassImpl =>
        val qname = c.qualifiedName
        val canonical = (if (qname == null || qname == c.name) c.name
                         else "_root_." + qname) + c.typeParamString
        testMethodTypes.contains(canonical)
    }.getOrElse(false)

}
