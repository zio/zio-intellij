package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyZipRightInspection

abstract class ZipRightInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyZipRightInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyFlatmapWithZipRightTest extends ZipRightInspectionTest(".zipRight") {

  def test_flatMap_to_zipRight(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").flatMap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipRight(x)""")
    testQuickFixes(text, result, hint)
  }

  def test_blockflatMap_to_zipRight(): Unit = {
    z {
      s"""ZIO.succeed("Remedios Varo").${START}flatMap { _ =>
         |  x
         |  x
         |  x
         |}$END""".stripMargin
    }.assertHighlighted()

    val text = z {
      """ZIO.succeed("Remedios Varo").flatMap { _ =>
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Remedios Varo").zipRight {
        | x
        | x
        | x
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}flatMap(x => x)$END""").assertNotHighlighted()

}

class SimplifyFlatmapWithZipRightOperatorTest extends ZipRightInspectionTest("*>") {

  def test_infix_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").flatMap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") *> (ZIO succeed x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_infix_flatMap_to_*>(): Unit = {
    z {
      s"""ZIO.succeed("Xul Solar").${START}flatMap { _ =>
         |  ZIO succeed x
         |  ZIO succeed x
         |  ZIO succeed x
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Xul Solar").flatMap { _ =>
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Xul Solar") *> {
        |  ZIO succeed x
        |  ZIO succeed x
        |  ZIO succeed x
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").flatMap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") *> ZIO.succeed(x)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_flatMap_to_*>(): Unit = {
    z {
      s"""ZIO.succeed("Frida Kahlo").${START}flatMap { _ =>
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |  ZIO.succeed(x)
         |}$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      """ZIO.succeed("Frida Kahlo").flatMap { _ =>
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    val result = z {
      """ZIO.succeed("Frida Kahlo") *> {
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |  ZIO.succeed(x)
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}flatMap(x => x)$END""").assertNotHighlighted()

}
