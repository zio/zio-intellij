package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{ SELECTION_END_TAG => END, SELECTION_START_TAG => START }
import zio.intellij.inspections.simplifications.SimplifyTapInspection

abstract class SimplifyTapInspectionTestBase(s: String) extends ZSimplifyInspectionTest[SimplifyTapInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyTapErrorInspectionTest extends SimplifyTapInspectionTestBase(".tapError") {

  def test_simplify_infix_zio_fail(): Unit = {
    z(s"ZIO.unit.${START}catchAll(ex => logError(ex) *> ZIO.fail(ex))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.catchAll(ex => logError(ex) *> ZIO.fail(ex))")
    val result = z(s"ZIO.unit.tapError(ex => logError(ex))")
    testQuickFix(text, result, hint)
  }
}

class SimplifyTapInspectionTest extends SimplifyTapInspectionTestBase(".tap") {

  def test_simplify_flatmap_as(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => f(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => f(a).as(a))")
    val result = z(s"ZIO.unit.tap(a => f(a))")
    testQuickFix(text, result, hint)
  }
}
