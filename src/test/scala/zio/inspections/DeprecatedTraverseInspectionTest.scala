package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.deprecations.DeprecatedTraverseInspection

abstract class DeprecatedTraverseInspectionTest(methodToReplace: String, methodToReplaceWith: String)
    extends ZInspectionTest[DeprecatedTraverseInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint: String = s"Replace with ZIO.$methodToReplaceWith"

  def testHighlighting(): Unit =
    z(s"${START}ZIO.$methodToReplace$END(myIterable)(f)").assertHighlighted()

  def testReplacement(): Unit = {
    val text   = z(s"ZIO.$methodToReplace(myIterable)(f)")
    val result = z(s"ZIO.$methodToReplaceWith(myIterable)(f)")
    testQuickFix(text, result, hint)
  }
}

class DeprecatedTraverseToForeachInspectionTest
    extends DeprecatedTraverseInspectionTest(methodToReplace = "traverse", methodToReplaceWith = "foreach")

class DeprecatedTraverseParToForeachParInspectionTest
    extends DeprecatedTraverseInspectionTest(methodToReplace = "traversePar", methodToReplaceWith = "foreachPar")

class DeprecatedTraverse_ToForeach_InspectionTest
    extends DeprecatedTraverseInspectionTest(methodToReplace = "traverse_", methodToReplaceWith = "foreach_")

class DeprecatedTraversePar_ToForeachPar_InspectionTest
    extends DeprecatedTraverseInspectionTest(methodToReplace = "traversePar_", methodToReplaceWith = "foreachPar_")
