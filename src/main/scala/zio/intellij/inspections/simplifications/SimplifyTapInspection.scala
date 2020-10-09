package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInsight.intention.expression.ConvertParameterToUnderscoreIntention
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.extensions.PsiElementExt
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.{createExpressionFromText, createWildcardNode}
import zio.intellij.inspections.zioMethods._
import zio.intellij.inspections.{ZInspection, _}
import zio.intellij.utils.isElementUsed
import zio.intellij.utils.StringUtils._

class SimplifyTapInspection
    extends ZInspection(
      FlatMapSimplificationType,
      FlatMapErrorSimplificationType,
      CatchAllToFailSimplificationType,
      TapBothSimplificationType
    )

object SimplifyTapInspection {

  // checks whether a method call was made on a lambda parameter
  private def callOnParameter(mc: ScMethodCall, param: ScParameter) =
    mc.firstChild match {
      case Some(r: ScReferenceExpression) =>
        r.smartQualifier match {
          case Some(ref: ScReference) => ref.isReferenceTo(param)
          case _                      => false
        }
      case _ => false
    }

  def simplifyFunctionCall(param: ScParameter, body: ScExpression): String = {
    def go(body: ScExpression, parent: Option[ScExpression]): String =
      (body, parent) match {
        case (ref: ScReferenceExpression, Some(func)) =>
          ref.firstChild match {
            case Some(mc: ScMethodCall) if !callOnParameter(mc, param) => go(mc, Some(body))
            case _ =>
              val expression = s"${param.name} => ${ref.getText}"
              createExpressionFromText(expression, func) match {
                case e: ScFunctionExpr =>
                  ConvertParameterToUnderscoreIntention
                    .createExpressionToIntroduce(e, true)
                    .swap
                    .fold(_ => replaceWithUnderscore(e).getText, _.getText)
              }
          }

        case (mc: ScMethodCall, Some(expr)) =>
          mc.argumentExpressions.toList match {
            case (ref: ScReference) :: Nil if ref.isReferenceTo(param) =>
              val invoked = mc.getEffectiveInvokedExpr
              if (invoked.textContains('.')) s"${param.getText} => ${mc.getText}" else invoked.getText
            case Nil => s"${param.getText} => ${expr.getText}"
            case _   => s"${param.getText} => ${body.getText}"
          }
        case _ => s"${param.getText} => ${body.getText}"
      }
    go(body, body.parentOfType[ScFunctionExpr])
  }

  private def replaceWithUnderscore(e: ScFunctionExpr): ScFunctionExpr = {
    val params = e.parameters.filterNot(p => p.isWildcard || p.isImplicitParameter)
    params.collectFirst {
      case named: ScNamedElement if !isElementUsed(named, false) =>
        val wildcard = createWildcardNode(e.getProject).getPsi
        named.nameId.replace(wildcard)
        e
      case _ => e
    }
      .getOrElse(e)
  }
}

sealed abstract class BaseRefactoringType(invocation: Qualified, replaceWith: String) extends SimplificationType {
  import SimplifyTapInspection._

  override def hint: String = s"Replace with .$replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, param: ScParameter, body: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"$replaceWith(${simplifyFunctionCall(param, body)})"))
        .highlightFrom(qual)

    expr match {
      case qual invocation lambda(Seq(param), body) =>
        body match {
          // .flatMap*(a => expr(e).as(a))
          case ref `.as` ScReferenceExpression(a) if a == param => Some(replacement(qual, param, ref))
          // .catchAll(e => expr(e) *> ZIO.fail(e))
          case ScInfixExpr(ref, _, `ZIO.fail`(_)) => Some(replacement(qual, param, ref))
          case _                                  => None
        }
      case _ => None
    }
  }
}

object FlatMapSimplificationType        extends BaseRefactoringType(`.flatMap`, "tap")
object FlatMapErrorSimplificationType   extends BaseRefactoringType(`.flatMapError`, "tapError")
object CatchAllToFailSimplificationType extends BaseRefactoringType(`.catchAll`, "tapError")

object TapBothSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .tapBoth"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression, tapError: ScExpression, tap: ScExpression) =
      replace(expr)
        .withText(invocationText(qual, s"tapBoth(${tapError.getBracedText}, ${tap.getBracedText})"))
        .highlightFrom(qual)

    expr match {
      case qual `.tap` tap `.tapError` tapError => Some(replacement(qual, tapError, tap))
      case qual `.tapError` tapError `.tap` tap => Some(replacement(qual, tapError, tap))
      case _                                    => None
    }
  }

}
