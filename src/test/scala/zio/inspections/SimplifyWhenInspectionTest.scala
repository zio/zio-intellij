package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyWhenInspection

class SimplifyWhenInspectionTest extends ZInspectionTest[SimplifyWhenInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .when"

  def test_when_reference_to_val(): Unit = {
    z(s"""|val a = true
          |val b = ZIO.succeed(42)
          |${START}if (a) b else ZIO.unit$END""".stripMargin).assertHighlighted()
    val text   = z(
      s"""|val a = true
          |val b = ZIO.succeed(42)
          |if (a) b else ZIO.unit""".stripMargin)
    val result = z(
      s"""|val a = true
          |val b = ZIO.succeed(42)
          |b.when(a)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_when_direct_reference(): Unit = {
    z(s"""|val a = true
          |${START}if (a) ZIO.succeed(42) else ZIO.unit$END""".stripMargin).assertHighlighted()
    val text   = z(
      s"""|val a = true
          |if (a) ZIO.succeed(42) else ZIO.unit""".stripMargin)
    val result = z(
      s"""|val a = true
          |ZIO.succeed(42).when(a)""".stripMargin)
    testQuickFix(text, result, hint)
  }
}
