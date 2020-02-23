package zio.intellij.inspections.deprecations

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScMethodCall, ScReferenceExpression}
import zio.intellij.inspections._

class DeprecatedTraverseInspection
    extends ZInspection(
      DeprecatedTraverseRefactoringType,
      DeprecatedTraverseParRefactoringType,
      DeprecatedTraverse_RefactoringType,
      DeprecatedTraversePar_RefactoringType
    )

sealed abstract class BaseDeprecatedTraverseRefactoringType(methodToReplace: String, methodToReplaceWith: String)
    extends SimplificationType {
  override def hint: String = s"Replace with ZIO.$methodToReplaceWith"

  // todo: figure out how to implement methodExtractor for methods with multiple parameter lists
  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    case ScMethodCall(ScMethodCall(ref @ ScReferenceExpression(_), Seq(_)), Seq(_))
        if fromZio(expr) && ref.refName == methodToReplace =>
      Some(replace(ref).withText(s"ZIO.$methodToReplaceWith"))
    case _ => None
  }
}

object DeprecatedTraverseRefactoringType
    extends BaseDeprecatedTraverseRefactoringType(methodToReplace = "traverse", methodToReplaceWith = "foreach")

object DeprecatedTraverseParRefactoringType
    extends BaseDeprecatedTraverseRefactoringType(methodToReplace = "traversePar", methodToReplaceWith = "foreachPar")

object DeprecatedTraverse_RefactoringType
    extends BaseDeprecatedTraverseRefactoringType(methodToReplace = "traverse_", methodToReplaceWith = "foreach_")

object DeprecatedTraversePar_RefactoringType
    extends BaseDeprecatedTraverseRefactoringType(methodToReplace = "traversePar_", methodToReplaceWith = "foreachPar_")
