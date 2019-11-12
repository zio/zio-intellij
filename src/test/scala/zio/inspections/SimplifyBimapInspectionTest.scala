package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyBimapInspection

class SimplifyBimapInspectionTest extends ZInspectionTest[SimplifyBimapInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .bimap"

  def test_map_mapError(): Unit = {
    z(s"ZIO.succeed(42).${START}map(a).mapError(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(a).mapError(b)")
    val result = z("ZIO.succeed(42).bimap(b, a)")
    testQuickFix(text, result, hint)
  }

  def test_map_asError(): Unit = {
    z(s"ZIO.succeed(42).${START}map(a).asError(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(a).asError(b)")
    val result = z("ZIO.succeed(42).bimap(b, a)")
    testQuickFix(text, result, hint)
  }

  def test_as_mapError(): Unit = {
    z(s"ZIO.succeed(42).${START}as(a).mapError(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).as(a).mapError(b)")
    val result = z("ZIO.succeed(42).bimap(b, a)")
    testQuickFix(text, result, hint)
  }

  def test_as_asError(): Unit = {
    z(s"ZIO.succeed(42).${START}as(a).asError(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).as(a).asError(b)")
    val result = z("ZIO.succeed(42).bimap(b, a)")
    testQuickFix(text, result, hint)
  }

  def test_mapError_map(): Unit = {
    z(s"ZIO.succeed(42).${START}mapError(a).map(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).mapError(a).map(b)")
    val result = z("ZIO.succeed(42).bimap(a, b)")
    testQuickFix(text, result, hint)
  }

  def test_mapError_as(): Unit = {
    z(s"ZIO.succeed(42).${START}mapError(a).as(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).mapError(a).as(b)")
    val result = z("ZIO.succeed(42).bimap(a, b)")
    testQuickFix(text, result, hint)
  }

  def test_asError_map(): Unit = {
    z(s"ZIO.succeed(42).${START}asError(a).map(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).asError(a).map(b)")
    val result = z("ZIO.succeed(42).bimap(a, b)")
    testQuickFix(text, result, hint)
  }

  def test_asError_as(): Unit = {
    z(s"ZIO.succeed(42).${START}asError(a).as(b)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).asError(a).as(b)")
    val result = z("ZIO.succeed(42).bimap(a, b)")
    testQuickFix(text, result, hint)
  }
}
