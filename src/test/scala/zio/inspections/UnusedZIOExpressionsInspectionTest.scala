package zio.inspections

import zio.intellij.inspections.mistakes.UnusedZIOExpressionsInspection

class UnusedZIOExpressionsInspectionTest extends ZScalaInspectionTest[UnusedZIOExpressionsInspection] {

  override protected val description = UnusedZIOExpressionsInspection.unusedZioExprMessage

  def test_two_sibling_effects(): Unit =
    z(s"""${START}putStrLn("")$END
         |ZIO.unit""".stripMargin).assertHighlighted()

  def test_two_asserts(): Unit =
    z(s"""${START}assert(true)(isTrue)$END
         |assert(true)(isTrue)""".stripMargin).assertHighlighted()

  def test_zio_effect_and_assert(): Unit =
    z(s"""${START}ZIO.unit$END
         |assert(true)(isTrue)""".stripMargin).assertHighlighted()

  def test_zio_effect_and_just_value(): Unit =
    z(s"""${START}ZIO.unit$END
         |1""".stripMargin).assertHighlighted()

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

class UnusedZIOSpecInspectionTest extends ZScalaInspectionTest[UnusedZIOExpressionsInspection] {

  override protected val description = UnusedZIOExpressionsInspection.unusedZioSpecMessage

  def test_two_tests(): Unit =
    z(s"""${START}test("first")(assertTrue(true))$END
         |test("second")(assertTrue(true))""".stripMargin).assertHighlighted()

  def test_two_suites(): Unit =
    z(s"""${START}suite("first")(test("first")(assertTrue(true)))$END
         |suite("second")(test("second")(assertTrue(true)))""".stripMargin).assertHighlighted()

  def test_suite_and_test(): Unit =
    z(s"""${START}suite("first")(test("first")(assertTrue(true)))$END
         |test("second")(assertTrue(true))""".stripMargin).assertHighlighted()

  def test_test_and_suite(): Unit =
    z(s"""${START}test("first")(assertTrue(true))$END
         |suite("second")(test("second")(assertTrue(true)))""".stripMargin).assertHighlighted()

  def test_test_and_just_value(): Unit =
    z(s"""${START}test("first")(assertTrue(true))$END
         |1""".stripMargin).assertHighlighted()

  def test_suite_with_tests_no_highlight(): Unit =
    z(s"""${START}suite("first")(
         |  test("first_1")(assertTrue(true)),
         |  test("first_2")(assertTrue(true))
         |)$END
         |""".stripMargin).assertNotHighlighted()

  def test_entire_scope_highlighted(): Unit =
    z(s"""${START}suite("first")(
         |  test("first_1")(assertTrue(true)),
         |  test("first_2")(assertTrue(true))
         |)$END
         |suite("second")(test("second")(assertTrue(true)))
         |""".stripMargin).assertHighlighted()

}
