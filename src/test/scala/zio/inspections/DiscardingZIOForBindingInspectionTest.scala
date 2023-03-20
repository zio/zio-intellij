package zio.inspections

import zio.intellij.inspections.mistakes.DiscardingZIOForBindingInspection

class DiscardingZIOForBindingInspectionTest extends ZScalaInspectionTest[DiscardingZIOForBindingInspection] {

  override protected val description = DiscardingZIOForBindingInspection.message

  private val hint = DiscardingZIOForBindingInspection.hint

  def test_discarding_an_effect(): Unit = {
    z(s"""
         |for {
         |  _ <- ZIO.succeed(1)
         |  ${START}_ = ZIO.succeed(1)$END
         |  _ <- ZIO.succeed(1)
         |} yield 1
         |""".stripMargin).assertHighlighted()

    val text = z(s"""
                    |for {
                    |  _ <- ZIO.succeed(1)
                    |  _ = ZIO.succeed(1)
                    |  _ <- ZIO.succeed(1)
                    |} yield 1
                    |""".stripMargin)

    val result = z(s"""
                      |for {
                      |  _ <- ZIO.succeed(1)
                      |  _ <- ZIO.succeed(1)
                      |  _ <- ZIO.succeed(1)
                      |} yield 1
                      |""".stripMargin)

    testQuickFix(text, result, hint)
  }

  def test_discarding_an_effect_ref(): Unit = {
    z(s"""
         |val effect: Task[Int] = ZIO.succeed(1)
         |for {
         |  _ <- ZIO.succeed(1)
         |  ${START}_ = effect$END
         |  _ <- ZIO.succeed(1)
         |} yield 1
         |""".stripMargin).assertHighlighted()

    val text = z(s"""
                    |val effect: Task[Int] = ZIO.succeed(1)
                    |for {
                    |  _ <- ZIO.succeed(1)
                    |  _ = effect
                    |  _ <- ZIO.succeed(1)
                    |} yield 1
                    |""".stripMargin)

    val result = z(s"""
                      |val effect: Task[Int] = ZIO.succeed(1)
                      |for {
                      |  _ <- ZIO.succeed(1)
                      |  _ <- effect
                      |  _ <- ZIO.succeed(1)
                      |} yield 1
                      |""".stripMargin)

    testQuickFix(text, result, hint)
  }

  def test_discarding_an_effect_block(): Unit = {
    z(s"""
         |for {
         |  _ <- ZIO.succeed(1)
         |  ${START}_ = {
         |    val i = 1
         |    ZIO.succeed(i)
         |  }$END
         |  _ <- ZIO.succeed(1)
         |} yield 1
         |""".stripMargin).assertHighlighted()

    val text = z(s"""
                    |for {
                    |  _ <- ZIO.succeed(1)
                    |  _ = {
                    |    val i = 1
                    |    ZIO.succeed(i)
                    |  }
                    |  _ <- ZIO.succeed(1)
                    |} yield 1
                    |""".stripMargin)

    val result = z(s"""
                      |for {
                      |  _ <- ZIO.succeed(1)
                      |  _ <- {
                      |    val i = 1
                      |    ZIO.succeed(i)
                      |  }
                      |  _ <- ZIO.succeed(1)
                      |} yield 1
                      |""".stripMargin)

    testQuickFix(text, result, hint)
  }

  def test_discarding_another_effect_no_highlight(): Unit =
    z(s"""
         |for {
         |  _ <- ZIO.succeed(1)
         |  ${START}_ = ZStream.succeed(1)$END
         |  _ <- ZIO.succeed(1)
         |} yield 1
         |""".stripMargin).assertNotHighlighted()

  def test_effect_binding_no_highlight(): Unit =
    z(s"""
         |for {
         |  _ <- ZIO.succeed(1)
         |  ${START}v = ZIO.succeed(1)$END
         |  _ <- ZIO.succeed(1)
         |} yield 1
         |""".stripMargin).assertNotHighlighted()

}
