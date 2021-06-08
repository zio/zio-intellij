package zio.intellij.testsupport

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiElementExt, ResolvesTo}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.ScClassImpl
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestFramework
import zio.intellij.testsupport.ZTestFramework.expandsToTestMethod

import scala.annotation.tailrec

final class ZTestFramework extends AbstractTestFramework {
  override def getMarkerClassFQName: String = ZSpecFQN
  override def getDefaultSuperClass: String = ZSpecFQN
  override def testFileTemplateName: String = "ZIO Test Suite"
  override def getName: String              = "ZIO Test"

  override def isTestMethod(element: PsiElement): Boolean = isTestMethod(element, checkAbstract = false)

  override def isTestMethod(element: PsiElement, checkAbstract: Boolean): Boolean =
    element match {
      case sc: ScReferenceExpression => resolvesToTestMethod(sc)
      case _                         => false
    }

  override protected def isTestClass(definition: ScTemplateDefinition): Boolean =
    if (!definition.is[ScObject]) false
    else super.isTestClass(definition)

  override def baseSuitePaths: Seq[String] = List(ZSpecFQN)

  private def resolvesToTestMethod(sc: ScReferenceExpression): Boolean =
    sc match {
      case ResolvesTo(f: ScFunctionDefinition) if !nested(sc) =>
        f.returnType match {
          case Right(returnType) if isOfClassFrom(returnType, Array("zio.test._")) =>
            expandsToTestMethod(returnType)
          case _ => false
        }
      case _ => false
    }

  private def nested(ref: ScReferenceExpression): Boolean = {
    // TODO hack. I don't know how to do this any better
    // Handles either an infix @@/other after the test (e.g. test(...) @@ ignore)
    // or any chained methods (e.g. test(...).provide(...))
    // by checking that the previous expression was a ZIO test. In that case -
    // it's a nested "test-like" method and is not considered for a line marker

    @tailrec
    def hasParentTestMethod(elem: Option[PsiElement]): Boolean =
      elem match {
        case Some(mc: ScMethodCall) if mc.`type`().exists(expandsToTestMethod) => true
        case Some(other)                                                       => hasParentTestMethod(other.firstChild)
        case None                                                              => false
      }

    val nestedElem = ref.parent.collect {
      case i: ScInfixExpr   => i.firstChild
      case p: ScPostfixExpr => p.firstChild
      case _: ScMethodCall  => ref.firstChild
    }.flatten

    hasParentTestMethod(nestedElem)
  }
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
