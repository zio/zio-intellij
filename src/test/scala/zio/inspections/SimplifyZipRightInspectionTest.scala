package zio.inspections

import zio.intellij.inspections.simplifications._

abstract class ZipRightInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyZipRightInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyFlatmapWithZipRightTest extends ZipRightInspectionTest(".zipRight") {

  def test_flatMap_to_zipRight(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").flatMap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipRight(x)""")
    testQuickFix(text, result, hint)
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
        |  x
        |  x
        |  x
        |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}flatMap(x => x)$END""").assertNotHighlighted()

}

class SimplifyFlatmapWithZipRightOperatorTest extends ZipRightInspectionTest("*>") {

  def test_infix_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").flatMap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") *> (ZIO succeed x)""")
    testQuickFix(text, result, hint)
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
    testQuickFix(text, result, hint)
  }

  def test_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").flatMap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") *> ZIO.succeed(x)""")
    testQuickFix(text, result, hint)
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
    testQuickFix(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit =
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}flatMap(x => x)$END""").assertNotHighlighted()

}

class SimplifyZipRightToSucceedInspectionTest extends ZSimplifyInspectionTest[SimplifyZipRightToSucceedInspection] {
  override protected val hint = s"Replace with .as"

  def test_zipRight_to_succeed(): Unit = {
    z(s"""${START}f("Fernando Fader").zipRight(ZIO.succeed("Carlos Alonso"))$END""").assertHighlighted()
    val text   = z("""f("Fernando Fader").zipRight(ZIO.succeed("Carlos Alonso"))""")
    val result = z("""f("Fernando Fader").as("Carlos Alonso")""")
    testQuickFix(text, result, hint)
  }

  def test_zipRight_infix_invocation_to_succeed(): Unit = {
    z(s"""${START}f("Fernando Fader") zipRight ZIO.succeed("Carlos Alonso")$END""").assertHighlighted()
    val text   = z("""f("Fernando Fader") zipRight ZIO.succeed("Carlos Alonso")""")
    val result = z("""f("Fernando Fader").as("Carlos Alonso")""")
    testQuickFix(text, result, hint)
  }

  def test_zipRight_operator_to_succeed(): Unit = {
    z(s"""${START}f("Fernando Fader") *> ZIO.succeed("Carlos Alonso")$END""").assertHighlighted()
    val text   = z("""f("Fernando Fader") *> ZIO.succeed("Carlos Alonso")""")
    val result = z("""f("Fernando Fader").as("Carlos Alonso")""")
    testQuickFix(text, result, hint)
  }

}
