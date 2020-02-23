package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.deprecations.DeprecatedTraverseInspection

abstract class DeprecatedTraverseInspectionTest(
  methodToReplace: String,
  methodToReplaceWith: String,
  isParN: Boolean = false
) extends ZInspectionTest[DeprecatedTraverseInspection] {
  import EditorTestUtil.{ SELECTION_END_TAG => END, SELECTION_START_TAG => START }

  private val params = if (isParN) "(n)(myIterable)(f)" else "(myIterable)(f)"

  override protected val hint: String = s"Replace with ZIO.$methodToReplaceWith"

  def testHighlighting(): Unit =
    z(s"${START}ZIO.$methodToReplace$END$params").assertHighlighted()

  def testReplacement(): Unit = {
    val text   = z(s"ZIO.$methodToReplace$params")
    val result = z(s"ZIO.$methodToReplaceWith$params")
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

class DeprecatedTraverseParNToForeachParNInspectionTest
    extends DeprecatedTraverseInspectionTest(
      methodToReplace = "traverseParN",
      methodToReplaceWith = "foreachParN",
      isParN = true
    )

class DeprecatedTraverseParN_ToForeachParN_InspectionTest
    extends DeprecatedTraverseInspectionTest(
      methodToReplace = "traverseParN_",
      methodToReplaceWith = "foreachParN_",
      isParN = true
    )
