package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{invocationText, Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScBlock, ScBlockStatement, ScExpression}
import zio.intellij.inspections.zioMethods.`.orElse`
import zio.intellij.inspections.{`ZIO.fail`, ZInspection}

class SimplifyOrElseInspection extends ZInspection(OrElseFailSimplificationType)

object OrElseFailSimplificationType extends SimplificationType {
  private val replaceWith = "orElseFail"

  override def hint: String = s"Replace with .$replaceWith"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def blockReplacement(qual: ScExpression, body: Seq[ScBlockStatement]): Simplification = {
      // new Intellij version doesn't seem to like Windows line separators
      // if ScalaPsiElementFactory.createBlockWithGivenExpressions can use "\n", so can we
      val separator = "\n"
      val blockBody = body.map(_.getText).mkString(separator, separator, separator)
      replace(expr).withText(s"${qual.getText}.$replaceWith {$blockBody}").highlightFrom(qual)
    }

    def replacement(qual: ScExpression, error: ScExpression): Simplification =
      replace(expr).withText(invocationText(qual, replaceWith, error)).highlightFrom(qual)

    expr match {
      case qual `.orElse` `ZIO.fail`(_, error) => Some(replacement(qual, error))
      case qual `.orElse` (block: ScBlock) =>
        Option(block.statements).collect {
          case statements :+ `ZIO.fail`(_, error) => blockReplacement(qual, statements :+ error)
        }
      case _ => None
    }
  }
}
