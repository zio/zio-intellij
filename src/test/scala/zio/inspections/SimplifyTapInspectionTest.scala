package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyTapInspection

class SimplifyTapInspectionTest extends ZSimplifyInspectionTest[SimplifyTapInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .tapError"

  def test_simplify_infix_zio_fail(): Unit = {
    z(s"ZIO.unit.${START}catchAll(ex => logError(ex) *> ZIO.fail(ex))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.catchAll(ex => logError(ex) *> ZIO.fail(ex))")
    val result = z(s"ZIO.unit.tapError(ex => logError(ex))")
    testQuickFix(text, result, hint)
  }
}
