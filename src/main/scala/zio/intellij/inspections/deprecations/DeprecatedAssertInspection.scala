package zio.intellij.inspections.deprecations

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.extensions.&&
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

class DeprecatedAssertInspection extends ZInspection(DeprecatedAssertRefactoringType)

object DeprecatedAssertRefactoringType extends SimplificationType {
  val ProblemText = "assert(value)(assertion)"

  override def hint: String = "Replace with assert(value)(assertion)"

  override def getSimplification(expr: ScExpression): Option[Simplification] = {
    def replacement(a: ScExpression, b: ScExpression) =
      replace(expr)
        .withText(s"assert(${a.getText})(${b.getText})")
        .highlightFrom(expr)

    expr match {
      case `assert`(a, b) && IsDeprecated(ann) if ann.getText.contains(ProblemText) => Some(replacement(a, b))
      case _                                                                        => None
    }
  }
}
