package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyZipLeftInspection

abstract class ZipLeftInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyZipLeftInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyTapWithZipLeftTest extends ZipLeftInspectionTest(".zipLeft") {

  def test_tap_to_zipLeft(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").tap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipLeft(x)""")
    testQuickFixes(text, result, hint)
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
    testQuickFixes(text, result, hint)
  }

  def test_tap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}tap(x => x)$END""").assertNotHighlighted()

}

class SimplifyTapWithZipLeftOperatorTest extends ZipLeftInspectionTest("<*") {

  def test_infix_tap_to_<*(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").tap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") <* (ZIO succeed x)""")
    testQuickFixes(text, result, hint)
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
    testQuickFixes(text, result, hint)
  }

  def test_tap_to_<*(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}tap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").tap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") <* ZIO.succeed(x)""")
    testQuickFixes(text, result, hint)
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
    testQuickFixes(text, result, hint)
  }

  def test_tap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}tap(x => x)$END""").assertNotHighlighted()

}
