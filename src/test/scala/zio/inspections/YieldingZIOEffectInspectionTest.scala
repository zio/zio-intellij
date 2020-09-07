package zio.inspections

import zio.intellij.inspections.mistakes.YieldingZIOEffectInspection

class YieldingZIOEffectInspectionTest extends ZScalaInspectionTest[YieldingZIOEffectInspection] {
  import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val description = YieldingZIOEffectInspection.message

  def test_yielding_an_effect(): Unit = z(s"""for {
                                             |  _ <- ZIO.succeed(1)
                                             |} yield ${START}ZIO.effect(2)$END
                                             |""".stripMargin).assertHighlighted()

  def test_yielding_an_curried_2_effect(): Unit = z(s"""for {
                                                       |  _ <- ZIO.succeed(1)
                                                       |} yield ${START}ZIO.foreach(List(2))(UIO(_))$END
                                                       |""".stripMargin).assertHighlighted()

  def test_yielding_an_curried_3_effect(): Unit = z(s"""for {
                                                       |  _ <- ZIO.succeed(1)
                                                       |} yield ${START}ZIO.foreachParN(1)(List(2))(UIO(_))$END
                                                       |""".stripMargin).assertHighlighted()

  def test_yielding_a_val_zio_reference_effect(): Unit = z(s"""val x: UIO[Int] = ???
                                                              |for {
                                                              |  _ <- ZIO.succeed(1)
                                                              |} yield ${START}x$END
                                                              |""".stripMargin).assertHighlighted()

  def test_yielding_a_def_zio_reference_effect(): Unit = z(s"""def x: UIO[Int] = ???
                                                              |for {
                                                              |  _ <- ZIO.succeed(1)
                                                              |} yield ${START}x$END
                                                              |""".stripMargin).assertHighlighted()

  def test_yielding_a_block(): Unit = z(s"""def x: UIO[Int] = ???
                                           |for {
                                           |  _ <- ZIO.succeed(1)
                                           |} yield {
                                           |  ${START}x$END
                                           |}
                                           |""".stripMargin).assertHighlighted()

  def test_yielding_a_block2(): Unit = z(s"""def x: UIO[Int] = ???
                                            |for {
                                            |  _ <- ZIO.succeed(1)
                                            |} yield {
                                            |  val y = {
                                            |    x
                                            |  }
                                            |  ${START}y$END
                                            |}
                                            |""".stripMargin).assertHighlighted()

}
