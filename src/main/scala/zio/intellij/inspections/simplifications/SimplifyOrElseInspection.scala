package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScBlockStatement, ScExpression}
import zio.intellij.inspections.zioMethods.`.orElse`
import zio.intellij.inspections.{ZInspection, `ZIO.fail`, invocationTextFor}

class SimplifyOrElseInspection extends ZInspection(OrElseFailSimplificationType)

object OrElseFailSimplificationType extends SimplificationType {
  private val replaceWith = "orElseFail"

  override def hint: String = s"Replace with .$replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def blockReplacement(zio: ScExpression, body: Seq[ScBlockStatement]): Simplification = {
      // new Intellij version doesn't seem to like Windows line separators
      // if ScalaPsiElementFactory.createBlockWithGivenExpressions can use "\n", so can we
      val separator = "\n"
      val blockBody = body.map(_.getText).mkString(separator, separator, separator)
      replace(expr).withText(s"${zio.getText}.$replaceWith {$blockBody}")
    }

    def replacement(zio: ScExpression, error: ScExpression): Simplification =
      replace(expr).withText(invocationTextFor(zio, replaceWith, error))

    expr match {
      case zio `.orElse` `ZIO.fail`(_, error) => Some(replacement(zio, error))
      case zio `.orElse` (block: ScBlock) =>
        Option(block.statements).collect {
          case statements :+ `ZIO.fail`(_, error) => blockReplacement(zio, statements :+ error)
        }
      case _ => None
    }
  }
}
