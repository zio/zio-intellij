package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFor, ScInfixExpr}
import zio.intellij.inspections._
import zio.intellij.inspections.simplifications.WhenUnlessSimplificationTypeBase._
import zio.intellij.utils.NegationUtils.{hasNegation, invertedNegationText, removeDoubleNegation}
import zio.intellij.utils.StringUtils._

class SimplifyWhenInspection   extends ZInspection(WhenSimplificationType, ZIOWhenSimplificationType)
class SimplifyUnlessInspection extends ZInspection(UnlessSimplificationType, ZIOUnlessSimplificationType)

sealed abstract class WhenUnlessSimplificationTypeBase(method: ReplacementMethod) extends SimplificationType {
  override val hint: String = s"Replace with $method"

  protected def replacement(ifStmt: ScExpression, body: ScExpression, conditionText: String): Simplification = {
    val replacementText = method match {
      case `ZIO.when` | `ZIO.unless` => s"$method($conditionText)${body.getWrappedText}"
      case `.when` | `.unless` =>
        val bodyText = body match {
          case _: ScInfixExpr | _: ScFor => body.getParenthesizedText
          case _                         => body.getText
        }
        s"$bodyText$method($conditionText)"
    }

    replace(ifStmt).withText(replacementText)
  }

  protected def replacement(
    ifStmt: ScExpression,
    body: ScExpression,
    condition: ScExpression,
    shouldHaveNegation: Boolean
  ): Option[Simplification] = {
    val conditionWithoutDoubleNegation = removeDoubleNegation(condition)
    if (hasNegation(conditionWithoutDoubleNegation))
      Option.when(shouldHaveNegation) {
        replacement(ifStmt, body, invertedNegationText(conditionWithoutDoubleNegation))
      }
    else
      Option.unless(shouldHaveNegation) {
        replacement(ifStmt, body, conditionWithoutDoubleNegation.getText)
      }
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case ifStmt @ IfStmt(condition, body @ zioLike(_), `ZIO.unit`(_, _)) =>
        replacement(ifStmt, body, condition, shouldHaveNegation = isUnlessLike)
      case ifStmt @ IfStmt(condition, `ZIO.unit`(_, _), body @ zioLike(_)) =>
        replacement(ifStmt, body, condition, shouldHaveNegation = isWhenLike)
      case _ => None
    }

  private val isWhenLike: Boolean = method match {
    case `.when` | `ZIO.when`     => true
    case `.unless` | `ZIO.unless` => false
  }

  private val isUnlessLike: Boolean = !isWhenLike

}

object WhenUnlessSimplificationTypeBase {

  sealed trait ReplacementMethod
  case object `.when`      extends ReplacementMethod
  case object `.unless`    extends ReplacementMethod
  case object `ZIO.when`   extends ReplacementMethod
  case object `ZIO.unless` extends ReplacementMethod

}

object WhenSimplificationType    extends WhenUnlessSimplificationTypeBase(`.when`)
object ZIOWhenSimplificationType extends WhenUnlessSimplificationTypeBase(`ZIO.when`)

object UnlessSimplificationType    extends WhenUnlessSimplificationTypeBase(`.unless`)
object ZIOUnlessSimplificationType extends WhenUnlessSimplificationTypeBase(`ZIO.unless`)
