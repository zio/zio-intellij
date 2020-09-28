package zio.intellij

import com.intellij.codeInsight.TestFrameworks
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiClass, PsiElement}
import com.intellij.testIntegration.TestFramework
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition

package object testsupport {
  val ZSuitePaths = List("zio.test.RunnableSpec")

  def parentTypeDefinition(e: PsiElement): Option[ScTypeDefinition] =
    Option(PsiTreeUtil.getParentOfType(e, classOf[ScTypeDefinition], false))

  def detectZTestFramework(c: PsiClass): Option[TestFramework] =
    Option(TestFrameworks.detectFramework(c)) match {
      case Some(framework: ZTestFramework) if framework.isTestClass(c) => Some(framework)
      case _                                                           => None
    }

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

    def unapply(element: PsiElement): Option[(ScTypeDefinition, Option[ScReferenceExpression])] = element match {
      case leaf: LeafPsiElement if leaf.getElementType == ScalaTokenTypes.tIDENTIFIER =>
        leaf.parent match {
          case Some(td: ScTypeDefinition)       => infoForClass(td)
          case Some(ref: ScReferenceExpression) => infoForExpr(ref)
          case _                                => None
        }
      case _ => None
    }

    private def infoForClass(td: ScTypeDefinition) =
      detectZTestFramework(td).map(_ => (td, None))

    private def infoForExpr(expr: ScReferenceExpression) =
      for {
        td  <- parentTypeDefinition(expr)
        fw  <- detectZTestFramework(td)
        ref <- Some(expr).filter(fw.isTestMethod)
      } yield (td, Some(ref))

  }
}
