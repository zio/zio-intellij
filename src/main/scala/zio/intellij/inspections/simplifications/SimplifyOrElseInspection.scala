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
    def blockReplacement(zio: ScExpression, body: Seq[ScBlockStatement]): Simplification = {
      val separator = System.lineSeparator
      val blockBody = body.map(_.getText).mkString(separator, separator, separator)
      replace(expr).withText(s"${zio.getText}.$replaceWith {$blockBody}")
    }

    def replacement(zio: ScExpression, error: ScExpression): Simplification =
      replace(expr).withText(invocationText(zio, replaceWith, error))

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
