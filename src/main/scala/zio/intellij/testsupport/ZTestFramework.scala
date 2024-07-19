package zio.intellij.testsupport

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.{ObjectExt, PsiElementExt, ResolvesTo}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef._
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.ScClassImpl
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.testingSupport.test.AbstractTestFramework
import zio.intellij.ZioIcon
import zio.intellij.inspections.isOfClassFrom
import zio.intellij.testsupport.ZTestFramework.{ZIO1SpecFQN, ZIO2SpecFQN, expandsToTestMethod}
import zio.intellij.utils.TypeCheckUtils.zioTestPackage

import javax.swing.Icon
import scala.annotation.tailrec

final class Zio1TestFramework extends ZTestFramework(ZIO1SpecFQN, "1.x")
final class Zio2TestFramework extends ZTestFramework(ZIO2SpecFQN, "2.x")

sealed abstract class ZTestFramework(zSpecFqn: String, zioVersion: String) extends AbstractTestFramework {
  override def getMarkerClassFQName: String = zSpecFqn
  override def getDefaultSuperClass: String = zSpecFqn
  override def testFileTemplateName: String = "ZIO Test Suite"
  override def getName: String              = s"ZIO Test ($zioVersion)"
  override def getIcon: Icon                = ZioIcon

  override def isTestMethod(element: PsiElement): Boolean = isTestMethod(element, checkAbstract = false)

  override def isTestMethod(element: PsiElement, checkAbstract: Boolean): Boolean =
    element match {
      case sc: ScReferenceExpression => resolvesToTestMethod(sc)
      case _                         => false
    }

  override protected def isTestClass(definition: ScTemplateDefinition): Boolean =
    if (!definition.is[ScObject]) false
    else super.isTestClass(definition)

  override def baseSuitePaths: Seq[String] = List(zSpecFqn)

  private def resolvesToTestMethod(sc: ScReferenceExpression): Boolean =
    sc match {
      case ResolvesTo(f: ScFunctionDefinition) if !nested(sc) =>
        f.returnType match {
          case Right(SpecReturnType(returnType)) if isOfClassFrom(returnType, zioTestPackage) =>
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

  object SpecReturnType {
    def unapply(tpe: ScType): Option[ScType] =
      // in ZIO 2.0 the spec return type was changed to a path-dependent type alias
      tpe.aliasType match {
        case Some(alias) => alias.upper.toOption
        case None        => Some(tpe)
      }

  }
}

object ZTestFramework {
  val ZIO1SpecFQN = "zio.test.AbstractRunnableSpec"
  val ZIO2SpecFQN = "zio.test.ZIOSpecAbstract"

  private[ZTestFramework] val testMethodTypes = Set(
    "_root_.zio.test.Spec[R, E]",
    "_root_.zio.test.Spec[R, E, T]",
    "_root_.zio.test.Spec[R, _root_.zio.test.TestFailure[E], _root_.zio.test.TestSuccess]",
    "_root_.zio.test.Spec[Any, _root_.zio.test.TestFailure[Nothing], _root_.zio.test.TestSuccess]",
    "_root_.zio.test.MutableRunnableSpec.SuiteBuilder",
    "_root_.zio.test.MutableRunnableSpec.TestBuilder"
  )

  private[ZTestFramework] val additionalMethodsRegex = Set(
    "_root_.zio.test.*.SuiteBuilder",
    "_root_.zio.test.*.TestBuilder"
  )

  private def expandsToTestMethod(tpe: ScType) =
    tpe.extractClass.collect {
      case c: ScClassImpl =>
        val qname = c.qualifiedName
        val canonical = (if (qname == null || qname == c.name) c.name
                         else "_root_." + qname) + c.typeParamString
        testMethodTypes.contains(canonical) || additionalMethodsRegex.exists(canonical.matches)
    }.getOrElse(false)

}
