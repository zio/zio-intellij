package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyFlatmapInspection

abstract class FlatmapInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyFlatmapInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyFlatmapWithZipRightTest extends FlatmapInspectionTest(".zipRight") {

  def test_flatMap_to_zipRight(): Unit = {
    z(s"""ZIO.succeed("Remedios Varo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Remedios Varo").flatMap(_ => x)""")
    val result = z("""ZIO.succeed("Remedios Varo").zipRight(x)""")
    testQuickFix(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit = {
    z(s"""ZIO.succeed("Tarsila do Amaral").${START}flatMap(x => x)$END""").assertNotHighlighted()
  }

}

class SimplifyFlatmapWithZipRightOperatorTest extends FlatmapInspectionTest("*>") {

  def test_infix_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Xul Solar").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Xul Solar").flatMap(_ => ZIO succeed x)""")
    val result = z("""ZIO.succeed("Xul Solar") *> (ZIO succeed x)""")
    testQuickFix(text, result, hint)
  }

  def test_flatMap_to_*>(): Unit = {
    z(s"""ZIO.succeed("Frida Kahlo").${START}flatMap(_ => x)$END""").assertHighlighted()
    val text   = z("""ZIO.succeed("Frida Kahlo").flatMap(_ => ZIO.succeed(x))""")
    val result = z("""ZIO.succeed("Frida Kahlo") *> ZIO.succeed(x)""")
    testQuickFix(text, result, hint)
  }

  def test_flatMap_not_discarding_should_not_highlight(): Unit = {
    z(s"""ZIO.succeed("Benito Quinquela Martín").${START}flatMap(x => x)$END""").assertNotHighlighted()
  }

}
