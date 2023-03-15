package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyErrorModificationInspection

abstract class SimplifyErrorModificationInspectionTest(toReplace: String, toReplaceWith: String)
    extends ZSimplifyInspectionTest[SimplifyErrorModificationInspection] {

  protected val methodToReplace     = s"$toReplace(_ => ???, f)"
  protected val methodToReplaceWith = s"$toReplaceWith(f)"

  override protected val hint = s"Replace with .$toReplaceWith"

  def testInlineHighlighting(): Unit =
    z(s"${START}ZIO.succeed(1).$methodToReplace$END").assertHighlighted()

  def testInlineReplacement(): Unit = {
    val text   = z(s"ZIO.succeed(1).$methodToReplace")
    val result = z(s"ZIO.succeed(1).$methodToReplaceWith")
    testQuickFix(text, result, hint)
  }

  def testValHighlighting(): Unit = z {
    s"""val foo = ZIO.succeed(1)
       |${START}foo.$methodToReplace$END""".stripMargin
  }.assertHighlighted()

  def testValReplacement(): Unit = {
    val text = z {
      s"""val foo = ZIO.succeed(1)
         |foo.$methodToReplace""".stripMargin
    }
    val result = z {
      s"""val foo = ZIO.succeed(1)
         |foo.$methodToReplaceWith""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testDefHighlighting(): Unit = z {
    s"""def foo = ZIO.succeed(1)
       |${START}foo.$methodToReplace$END""".stripMargin
  }.assertHighlighted()

  def testDefReplacement(): Unit = {
    val text = z {
      s"""def foo = ZIO.succeed(1)
         |foo.$methodToReplace""".stripMargin
    }
    val result = z {
      s"""def foo = ZIO.succeed(1)
         |foo.$methodToReplaceWith""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"${START}Task(1).$methodToReplace$END").assertNotHighlighted()

}

class SimplifyMapBothErrorModificationInspectionTest extends SimplifyErrorModificationInspectionTest("mapBoth", "map")
class SimplifyTapBothErrorModificationInspectionTest extends SimplifyErrorModificationInspectionTest("tapBoth", "tap")
class SimplifyFoldErrorModificationInspectionTest extends SimplifyErrorModificationInspectionTest("fold", "map") {
  def testInfallibleStreamNoHighlight(): Unit =
    z(s"${START}ZStream.succeed(1).$methodToReplace$END").assertNotHighlighted()
}

class SimplifyFoldMErrorModificationInspectionTest extends SimplifyErrorModificationInspectionTest("foldM", "flatMap") {
  def testInfallibleStreamNoHighlight(): Unit =
    z(s"${START}ZStream.succeed(1).$methodToReplace$END").assertNotHighlighted()
}
class SimplifyFoldZIOErrorModificationInspectionTest
    extends SimplifyErrorModificationInspectionTest("foldZIO", "flatMap") {
  override def isZIO1: Boolean = false
  def testInfallibleStreamNoHighlight(): Unit =
    z(s"${START}ZStream.succeed(1).$methodToReplace$END").assertNotHighlighted()
}

class SimplifyFoldTraceMErrorModificationInspectionTest
    extends SimplifyErrorModificationInspectionTest("foldTraceM", "flatMap")
class SimplifyFoldTraceZIOErrorModificationInspectionTest
    extends SimplifyErrorModificationInspectionTest("foldTraceZIO", "flatMap") {
  override def isZIO1: Boolean = false
}
