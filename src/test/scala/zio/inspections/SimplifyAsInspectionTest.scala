package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyMapInspection

abstract class MapInspectionTest(s: String) extends ZInspectionTest[SimplifyMapInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyMapTest extends MapInspectionTest(".as") {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  def test_map_to_x(): Unit = {
    z(s"ZIO.succeed(42).${START}map(_ => x)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(_ => x)")
    val result = z("ZIO.succeed(42).as(x)")
    testQuickFix(text, result, hint)
  }
}

class SimplifyMapErrorTest extends MapInspectionTest(".orElseFail") {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  def test_mapError_to_x(): Unit = {
    z(s"ZIO.succeed(42).${START}mapError(_ => x)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).mapError(_ => x)")
    val result = z("ZIO.succeed(42).orElseFail(x)")
    testQuickFix(text, result, hint)
  }
}
