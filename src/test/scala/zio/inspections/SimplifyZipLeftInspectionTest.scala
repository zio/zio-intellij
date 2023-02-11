package zio.inspections

import zio.intellij.inspections.simplifications._

abstract class ZipLeftInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyZipLeftInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyTapWithZipLeftTest extends ZipLeftInspectionTest(".zipLeft") {

  def test_tap_to_zipLeft(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").tap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipLeft(x)""")
    testQuickFix(text, result, hint)
  }

  def test_block_tap_to_zipLeft(): Unit = {
    z {
      s"""ZIO.succeed("Remedios Varo").${START}tap { _ =>
         |  x
         |  x
         |  x
         |}$END""".stripMargin
    }.assertHighlighted()

    val text = z {
      """ZIO.succeed("Remedios Varo").tap { _ =>
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Remedios Varo").zipLeft {
        | x
        | x
        | x
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_tap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}tap(x => x)$END""").assertNotHighlighted()

}

class SimplifyTapWithZipLeftOperatorTest extends ZipLeftInspectionTest("<*") {

  def test_infix_tap_to_<*(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").tap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") <* (ZIO succeed x)""")
    testQuickFix(text, result, hint)
  }

  def test_block_infix_tap_to_<*(): Unit = {
    z {
      s"""ZIO.succeed("Xul Solar").${START}tap { _ =>
         |  ZIO succeed x
         |  ZIO succeed x
         |  ZIO succeed x
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Xul Solar").tap { _ =>
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Xul Solar") <* {
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_tap_to_<*(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").tap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") <* ZIO.succeed(x)""")
    testQuickFix(text, result, hint)
  }

  def test_block_tap_to_<*(): Unit = {
    z {
      s"""ZIO.succeed("Frida Kahlo").${START}tap { _ =>
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Frida Kahlo").tap { _ =>
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Frida Kahlo") <* {
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_tap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}tap(x => x)$END""").assertNotHighlighted()

}

class SimplifySucceedToZipLeftInspectionTest extends ZSimplifyInspectionTest[SimplifySucceedToZipLeftInspection] {
  override protected val hint = s"Replace with .as"

  def test_succeed_to_zipLeft(): Unit = {
    z(s"""${START}ZIO.succeed("Fernando Fader").zipLeft(f("Carlos Alonso"))$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Fernando Fader").zipLeft(f("Carlos Alonso"))""")
    val result = z("""f("Carlos Alonso").as("Fernando Fader")""")
    testQuickFix(text, result, hint)
  }

  def test_zipLeft_infix_invocation_to_succeed(): Unit = {
    z(s"""${START}ZIO.succeed("Fernando Fader") zipLeft f("Carlos Alonso")$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Fernando Fader") zipLeft f("Carlos Alonso")""")
    val result = z("""f("Carlos Alonso").as("Fernando Fader")""")
    testQuickFix(text, result, hint)
  }

  def test_zipLeft_operator_to_succeed(): Unit = {
    z(s"""${START}ZIO.succeed("Fernando Fader") <* f("Carlos Alonso")$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Fernando Fader") <* f("Carlos Alonso")""")
    val result = z("""f("Carlos Alonso").as("Fernando Fader")""")
    testQuickFix(text, result, hint)
  }

}
