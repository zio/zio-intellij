package zio.inspections

import zio.intellij.inspections.mistakes.{DiscardingZIOValueInspection, DiscardingZIOValueSmartInspection}

class DiscardingZIOValueInspectionMapTest extends ZScalaInspectionTest[DiscardingZIOValueInspection] {

  override protected val description: String = DiscardingZIOValueInspection.mapDiscardMsg

  def test_discard_different_types_map(): Unit =
    z(s"val r: UIO[Unit] = ZIO.succeed(1).map(${START}ZStream.succeed(_)$END)").assertHighlighted()

  def test_no_explicit_discard_different_types_map_no_highlight(): Unit =
    z(s"val r = ZIO.succeed(1).map(${START}ZStream.succeed(_)$END)").assertNotHighlighted()

  // should be handled by smart inspection
  def test_discard_same_types_map_no_highlight(): Unit =
    z(s"val r: UIO[Unit] = ZIO.succeed(1).map(${START}ZIO.succeed(_)$END)").assertNotHighlighted()

}

class DiscardingZIOValueInspectionRunDrainTest extends ZScalaInspectionTest[DiscardingZIOValueInspection] {

  override protected val description: String = DiscardingZIOValueInspection.runDrainDiscardMsg

  def test_discard_zio_in_runDrain(): Unit =
    z(s"""val _ = true // to make test pass and not deal with braces which will not be an issue in real world
         |${START}ZStream.succeed(1).map(ZIO.succeed(_)).runDrain$END""".stripMargin).assertHighlighted()

  def test_discard_stream_in_runDrain(): Unit =
    z(s"""val _ = true // to make test pass and not deal with braces which will not be an issue in real world
         |${START}ZStream.succeed(1).map(ZStream.succeed(_)).runDrain$END""".stripMargin).assertHighlighted()

  def test_discard_other_in_runDrain_no_highlight(): Unit =
    z(s"""val _ = true // to make test pass and not deal with braces which will not be an issue in real world
         |${START}ZStream.succeed(1).map(Option(_)).runDrain$END""".stripMargin).assertNotHighlighted()

}

class DiscardingZIOValueSmartInspectionMapTest extends ZScalaInspectionTest[DiscardingZIOValueSmartInspection] {

  override protected val description: String = "Possibly mistaken value discarding. Perhaps you meant to use .flatMap?"

  def test_discard_zio(): Unit = {
    z(s"val r: UIO[Unit] = ZIO.succeed(1).${START}map(ZIO.debug(_))$END").assertHighlighted()

    val text   = z("val r: UIO[Unit] = ZIO.succeed(1).map(ZIO.debug(_))")
    val result = z("val r: UIO[Unit] = ZIO.succeed(1).flatMap(ZIO.debug(_))")

    testQuickFix(text, result, description)
  }

  def test_discard_stream(): Unit = {
    z(s"val r: UStream[Unit] = ZStream.succeed(1).${START}map(ZStream.succeed(_))$END").assertHighlighted()

    val text   = z("val r: UStream[Unit] = ZStream.succeed(1).map(ZStream.succeed(_))")
    val result = z("val r: UStream[Unit] = ZStream.succeed(1).flatMap(ZStream.succeed(_))")

    testQuickFix(text, result, description)
  }

  // should be handled by regular inspection
  def test_discard_different_types_no_highlight(): Unit =
    z(s"val r: UIO[Unit] = ZIO.succeed(1).${START}map(ZStream.succeed(_))$END").assertNotHighlighted()

  def test_no_explicit_discard_no_highlight(): Unit =
    z(s"val r = ZIO.succeed(1).${START}map(ZIO.succeed(_))$END").assertNotHighlighted()

  def test_discard_lambda(): Unit = {
    z(s"""
         |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
         |val r: UIO[Unit] = ZIO.succeed(1).${START}map(i => foo(i))$END""".stripMargin).assertHighlighted()

    val text = z(s"""
                    |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
                    |val r: UIO[Unit] = ZIO.succeed(1).map(i => foo(i))""".stripMargin)

    val result = z(s"""
                      |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
                      |val r: UIO[Unit] = ZIO.succeed(1).flatMap(i => foo(i))""".stripMargin)

    testQuickFix(text, result, description)
  }

  def test_discard_method(): Unit = {
    z(s"""
         |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
         |val r: UIO[Unit] = ZIO.succeed(1).${START}map(foo(_))$END""".stripMargin).assertHighlighted()

    val text = z(s"""
                    |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
                    |val r: UIO[Unit] = ZIO.succeed(1).map(foo(_))""".stripMargin)

    val result = z(s"""
                      |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
                      |val r: UIO[Unit] = ZIO.succeed(1).flatMap(foo(_))""".stripMargin)

    testQuickFix(text, result, description)
  }

  def test_discard_ref(): Unit = {
    z(s"""
         |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
         |val r: UIO[Unit] = ZIO.succeed(1).${START}map(foo)$END""".stripMargin).assertHighlighted()

    val text = z(s"""
                    |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
                    |val r: UIO[Unit] = ZIO.succeed(1).map(foo)""".stripMargin)

    val result = z(s"""
                      |def foo(a: Any): UIO[Unit] = ZIO.debug(a)
                      |val r: UIO[Unit] = ZIO.succeed(1).flatMap(foo)""".stripMargin)

    testQuickFix(text, result, description)
  }

}
