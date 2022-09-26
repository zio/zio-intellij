package zio.intellij.utils

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.extensions.{BooleanExt, ObjectExt}
import org.jetbrains.plugins.scala.lang.psi.api.expr._

import scala.annotation.tailrec

// Borrowed from org.jetbrains.plugins.scala.codeInsight.hints.methodChains
object MethodChain {
  def unapply(element: PsiElement): Option[Seq[ScExpression]] =
    element match {
      case expr: ScExpression if isMostOuterExpression(expr) =>
        Some(collectChain(expr))
      case _ => None
    }

  private def isMostOuterExpression(expr: PsiElement): Boolean =
    expr.getParent match {
      case _: ScReferenceExpression | _: MethodInvocation | _: ScGenericCall | _: ScParenthesisedExpr => false
      case _                                                                                          => true
    }

  private def collectChain(expr: ScExpression): List[ScExpression] = {
    @tailrec
    def collectChainAcc(expr: ScExpression, acc: List[ScExpression]): List[ScExpression] = {
      val newAcc = if (expr.getParent.is[ScMethodCall]) acc else expr :: acc
      expr match {
        case ScInfixExpr(left, _, _) => collectChainAcc(left, newAcc)
        case ChainCall(inner)        => collectChainAcc(inner, newAcc)
        case _                       => newAcc
      }
    }
    collectChainAcc(expr, Nil)
  }

  private object ChainCall {
    def unapply(element: PsiElement): Option[ScExpression] = element match {
      case ScReferenceExpression.withQualifier(inner) => Some(inner)
      case invoc: MethodInvocation                    => Some(invoc.getEffectiveInvokedExpr)
      case genericCall: ScGenericCall                 => Some(genericCall.referencedExpr)
      case ScParenthesisedExpr(inner)                 => Some(inner)
      case _                                          => None
    }
  }
}
