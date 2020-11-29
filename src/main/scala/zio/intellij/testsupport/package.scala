package zio.intellij

import com.intellij.codeInsight.TestFrameworks
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.search.searches.ReferencesSearch
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiClass, PsiElement}
import com.intellij.testIntegration.TestFramework
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScLiteral, ScReference}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition

import scala.collection.JavaConverters._

package object testsupport {
  val ZSuitePaths = List("zio.test.RunnableSpec")

  def parentTypeDefinition(e: PsiElement): Option[ScTypeDefinition] =
    parentOfType(e, classOf[ScTypeDefinition], strict = false)

  def detectZTestFramework(c: PsiClass): Option[TestFramework] =
    Option(TestFrameworks.detectFramework(c)) collect {
      case framework: ZTestFramework if framework.isTestClass(c) => framework
    }

  private def parentOfType[T <: PsiElement](
    elem: PsiElement,
    parentClass: Class[T],
    strict: Boolean = true
  ): Option[T] =
    Option(PsiTreeUtil.getParentOfType(elem, parentClass, strict))

  object testName {

    def unapply(expr: ScReferenceExpression): Option[String] =
      expr.parent match {
        case Some(m: ScMethodCall) =>
          m.argumentExpressions.headOption.flatMap {
            case lit: ScLiteral => Option(lit.getValue()).map(_.toString)
            case _              => None
          }
        case _ => None
      }
  }

  object IsZioTestElement {

    def unapply(element: PsiElement): Option[(ScTypeDefinition, Option[ScReferenceExpression])] =
      element match {
        case leaf: LeafPsiElement if leaf.getElementType == ScalaTokenTypes.tIDENTIFIER =>
          leaf.parent match {
            case Some(td: ScTypeDefinition)       => infoForClass(td)
            case Some(ref: ScReferenceExpression) => infoForLocalExpr(ref) // orElse infoForExprFromAnotherClass(ref)
            case _                                => None
          }
        case _ => None
      }

    private def infoForClass(td: ScTypeDefinition) =
      detectZTestFramework(td).map(_ => (td, None))

    private def infoForReferencedExpr(reference: ScReference, referencedExpr: ScReferenceExpression) =
      for {
        td  <- parentTypeDefinition(reference)
        fw  <- detectZTestFramework(td)
        ref <- Some(referencedExpr).filter(fw.isTestMethod)
      } yield (td, Some(ref))

    private def infoForLocalExpr(expr: ScReferenceExpression) =
      infoForReferencedExpr(expr, expr)

    private def infoForExprFromAnotherClass(expr: ScReferenceExpression) = {
      val refs = referencesOfPatternDef(expr)
        .orElse(referencesOfFunctionDef(expr))
        .getOrElse(Seq.empty)

      refs.flatMap {
        case ref: ScReferenceExpression => infoForReferencedExpr(ref.getElement, expr)
        case _                          => None
      }.headOption
    }

    private def findAllReferences(elem: PsiElement) =
      ReferencesSearch.search(elem, elem.getUseScope).findAll().asScala

    private def referencesOfPatternDef(elem: PsiElement) =
      parentOfType(elem, classOf[ScPatternDefinition])
        .map(_.bindings.flatMap(findAllReferences))

    private def referencesOfFunctionDef(elem: PsiElement) =
      parentOfType(elem, classOf[ScFunctionDefinition])
        .map(findAllReferences)
  }
}
