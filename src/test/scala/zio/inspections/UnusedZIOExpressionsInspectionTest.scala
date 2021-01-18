package zio.inspections

import zio.intellij.inspections.mistakes.UnusedZIOExpressionsInspection

class UnusedZIOExpressionsInspectionTest extends ZScalaInspectionTest[UnusedZIOExpressionsInspection] {
  import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val description = UnusedZIOExpressionsInspection.message

  def test_two_sibling_effects(): Unit =
    z(s"""${START}putStrLn("")$END
         |ZIO.unit""".stripMargin).assertHighlighted()

  def test_two_asserts(): Unit =
    z(s"""${START}assert(true)(isTrue)$END
         |assert(true)(isTrue)""".stripMargin).assertHighlighted()

  def test_zio_effect_and_assert(): Unit =
    z(s"""${START}ZIO.unit$END
         |assert(true)(isTrue)""".stripMargin).assertHighlighted()

  def test_entire_scope_highlighted(): Unit =
    z(s"""${START}ZIO.when(true) {
         |  putStrLn("Inventory is empty!") *>
         |  ZIO.unit
         |}$END
         |ZIO.succeed(42)
         |""".stripMargin).assertHighlighted()

  def test_unused_method_call_with_multiple_params(): Unit =
    z(s"""def x(one: Int, two: Int, three: Int): UIO[Int] = ???
         |${START}x(1, 2, 3)$END
         |UIO(1)
         |""".stripMargin).assertHighlighted()

  def test_prefix_operator_should_not_be_highlighted(): Unit =
    z(s"""$START!${END}zio.test.assertCompletes""").assertNotHighlighted()

}
