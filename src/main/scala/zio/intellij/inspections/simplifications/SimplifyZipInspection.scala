package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScInfixExpr}
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.StringUtils._

class SimplifyZipRightInspection          extends ZInspection(ZipRightSimplificationType, ZipRightOperatorSimplificationType)
class SimplifyZipRightToSucceedInspection extends ZInspection(ZipRightToSucceedSimplificationType)
class SimplifyZipLeftInspection           extends ZInspection(ZipLeftSimplificationType, ZipLeftOperatorSimplificationType)
class SimplifySucceedToZipLeftInspection  extends ZInspection(ZipLeftToSucceedSimplificationType)

sealed class BaseZipOneSimplificationType(invocation: Qualified, replaceWith: String) extends SimplificationType {

  override def hint: String = s"Replace with .$replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual invocation `_ => x`(x) =>
        Some(replace(expr).withText(invocationTextFor(qual, s"$replaceWith${x.getWrappedText}")))
      case _ => None
    }

}

sealed class BaseZipOneOperatorSimplificationType(invocation: Qualified, replaceWith: String)
    extends SimplificationType {

  override def hint: String = s"Replace with $replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {

    def replacement(qual: ScExpression, x: ScExpression) =
      x match {
        case _: ScInfixExpr => replace(expr).withText(s"${qual.getBracedText} $replaceWith (${x.getBracedText})")
        case _              => replace(expr).withText(s"${qual.getBracedText} $replaceWith ${x.getBracedText}")
      }

    expr match {
      case qual invocation `_ => x`(x) => Some(replacement(qual, x))
      case _                           => None
    }
  }

}

sealed abstract class BaseZipToSucceedSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .as"

  /**
   * Simplifies the `toSimplify` expression, replacing it with a `toKeep` expression followed by `.as(zioArg)`
   *
   * @param toSimplify Expression to simplify (full one)
   * @param toKeep Subexpression to keep
   * @param zioArg Argument to the `ZIO.succeed()` call
   * @return a SimplificationBuilder
   */
  protected def simplify(toSimplify: ScExpression, toKeep: ScExpression, zioArg: ScExpression): SimplificationBuilder =
    replace(toSimplify).withText(invocationTextFor(toKeep, s"as${zioArg.getWrappedText}")).highlightAll
}

object ZipRightSimplificationType         extends BaseZipOneSimplificationType(`.flatMap`, "zipRight")
object ZipRightOperatorSimplificationType extends BaseZipOneOperatorSimplificationType(`.flatMap`, "*>")
object ZipRightToSucceedSimplificationType extends BaseZipToSucceedSimplificationType {
  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case qual `.*>` `ZIO.succeed`(_, arg)       => Some(simplify(expr, qual, arg))
    case qual `.zipRight` `ZIO.succeed`(_, arg) => Some(simplify(expr, qual, arg))
    case _                                      => None
  }
}

object ZipLeftSimplificationType         extends BaseZipOneSimplificationType(`.tap_notStream`, "zipLeft")
object ZipLeftOperatorSimplificationType extends BaseZipOneOperatorSimplificationType(`.tap_notStream`, "<*")
object ZipLeftToSucceedSimplificationType extends BaseZipToSucceedSimplificationType {
  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case `ZIO.succeed`(_, arg) `.<*` qual      => Some(simplify(expr, qual, arg))
    case `ZIO.succeed`(_, arg) `.zipLeft` qual => Some(simplify(expr, qual, arg))
    case _                                     => None
  }
}
