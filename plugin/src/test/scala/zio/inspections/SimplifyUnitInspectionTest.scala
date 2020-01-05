package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyUnitInspection

class SimplifyUnitInspectionTest extends ZInspectionTest[SimplifyUnitInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .unit"

  def test_zipRight_method(): Unit = {
    z(s"ZIO.succeed(42).$START*>(ZIO.unit)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).*>(ZIO.unit)")
    val result = z("ZIO.succeed(42).unit")
    testQuickFix(text, result, hint)
  }

  def test_zipRight_infix(): Unit = {
    z(s"ZIO.succeed(42) $START*> ZIO.unit$END").assertHighlighted()
    val text   = z("ZIO.succeed(42) *> ZIO.unit")
    val result = z("ZIO.succeed(42).unit")
    testQuickFix(text, result, hint)
  }

  def test_as_unit(): Unit = {
    z(s"ZIO.succeed(42).${START}as(())$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).as(())")
    val result = z("ZIO.succeed(42).unit")
    testQuickFix(text, result, hint)
  }

  def test_map_to_unit(): Unit = {
    z(s"ZIO.succeed(42).${START}map(_ => ())$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(_ => ())")
    val result = z("ZIO.succeed(42).unit")
    testQuickFix(text, result, hint)
  }
}
