package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyFlattenInspection

class SimplifyFlattenInspectionTest extends ZSimplifyInspectionTest[SimplifyFlattenInspection] {

  override protected val hint = "Replace .map and .flatten with .flatMap"

  def test_map_flatten_flatMap(): Unit = {
    z(s"b.${START}map(x => ZIO.succeed(x * 2)).flatten$END").assertHighlighted()
    val text   = z("b.map(x => ZIO.succeed(x * 2)).flatten")
    val result = z("b.flatMap(x => ZIO.succeed(x * 2))")
    testQuickFix(text, result, hint)
  }
}
