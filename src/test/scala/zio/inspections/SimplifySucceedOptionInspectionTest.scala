package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifySucceedOptionInspection

abstract class SimplifyOptionInspectionTest(s: String)
    extends ZSimplifyInspectionTest[SimplifySucceedOptionInspection] {
  override protected val hint = s"Replace with $s"
}

class SucceedNoneInspectionTest extends SimplifyOptionInspectionTest("ZIO.none") {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  def test_succeed_None(): Unit = {
    z(s"${START}ZIO.succeed(None)$END").assertHighlighted()
    val text   = z("ZIO.succeed(None)")
    val result = z("ZIO.none")
    testQuickFix(text, result, hint)
  }
}

class SucceedSomeInspectionTest extends SimplifyOptionInspectionTest("ZIO.some") {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  def test_succeed_Some(): Unit = {
    z(s"${START}ZIO.succeed(Some(a))$END").assertHighlighted()
    val text   = z("ZIO.succeed(Some(a))")
    val result = z("ZIO.some(a)")
    testQuickFix(text, result, hint)
  }
}
