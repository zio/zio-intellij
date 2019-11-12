package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyIgnoreInspection

class SimplifyIgnoreInspectionTest extends ZInspectionTest[SimplifyIgnoreInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .ignore"

  def test_catchAll_ZIOUnit(): Unit = {
    z(s"ZIO.succeed(42).${START}catchAll(_ => ZIO.unit)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).catchAll(_ => ZIO.unit)")
    val result = z("ZIO.succeed(42).ignore")
    testQuickFix(text, result, hint)
  }

  def test_foldCause_unit(): Unit = {
    z(s"ZIO.succeed(42).${START}foldCause(_ => (), _ => ())$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).foldCause(_ => (), _ => ())")
    val result = z("ZIO.succeed(42).ignore")
    testQuickFix(text, result, hint)
  }

  def test_foldCauseM_ZIOUnit(): Unit = {
    z(s"ZIO.succeed(42).${START}foldCauseM(_ => ZIO.unit, _ => ZIO.unit)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).foldCauseM(_ => ZIO.unit, _ => ZIO.unit)")
    val result = z("ZIO.succeed(42).ignore")
    testQuickFix(text, result, hint)
  }
}
