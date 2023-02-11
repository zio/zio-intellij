package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyMapInspection

abstract class MapInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyMapInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyMapTest extends MapInspectionTest(".as") {

  def test_map_to_x(): Unit = {
    z(s"ZIO.succeed(42).${START}map(_ => x)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).map(_ => x)")
    val result = z("ZIO.succeed(42).as(x)")
    testQuickFix(text, result, hint)
  }

  def test_block_map_to_x(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}map { _ =>
         |  x
         |  x
         |  x
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).map { _ =>
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).as {
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }
}

class SimplifyMapErrorTest extends MapInspectionTest(".orElseFail") {

  def test_mapError_to_x(): Unit = {
    z(s"ZIO.succeed(42).${START}mapError(_ => x)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).mapError(_ => x)")
    val result = z("ZIO.succeed(42).orElseFail(x)")
    testQuickFix(text, result, hint)
  }

  def test_block_mapError_to_x(): Unit = {
    z {
      s"""ZIO.succeed(42).${START}mapError { _ =>
         |  x
         |  x
         |  x
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed(42).mapError { _ =>
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed(42).orElseFail {
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }
}
