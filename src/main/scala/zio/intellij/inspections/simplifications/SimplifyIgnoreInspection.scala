package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class SimplifyIgnoreInspection extends ZInspection(IgnoreSimplificationType)

object IgnoreSimplificationType extends SimplificationType {
  override def hint: String = "Replace with .ignore"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(qual: ScExpression) = replace(expr).withText(invocationTextFor(qual, "ignore"))
    expr match {
      case qual `.catchAll` `_ => ZIO.unit`()                          => Some(replacement(qual))
      case qual `.foldCause` (`_ => ()`(), `_ => ()`())                => Some(replacement(qual))
      case qual `.foldCauseM` (`_ => ZIO.unit`(), `_ => ZIO.unit`())   => Some(replacement(qual))
      case qual `.foldCauseZIO` (`_ => ZIO.unit`(), `_ => ZIO.unit`()) => Some(replacement(qual))
      case `.unit`(`.either`(qual))                                    => Some(replacement(qual).highlightFrom(qual))
      case _                                                           => None
    }
  }
}
