package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyOrElseInspection

class SimplifyOrElseInspectionTest extends ZSimplifyInspectionTest[SimplifyOrElseInspection] {

  override protected val hint = s"Replace with .orElseFail"

  def testOneLineHighlighting(): Unit =
    z {
      s"""b.orElse(b).${START}orElse(ZIO.fail(new RuntimeException("ooof")))$END"""
    }.assertHighlighted()

  def testOneLineReplacement(): Unit = {
    val text = z {
      """b.orElse(b).orElse(ZIO.fail(new RuntimeException("ooof")))"""
    }
    val result = z {
      """b.orElse(b).orElseFail(new RuntimeException("ooof"))"""
    }
    testQuickFixes(text, result, hint)
  }

  def testOneLineBlockHighlighting(): Unit =
    z {
      s"""b.orElse(b).${START}orElse {
         |  ZIO.fail(new RuntimeException("ooof"))
         |}$END""".stripMargin
    }.assertHighlighted()

  def testOneLineBlockReplacement(): Unit = {
    val text = z {
      """b.orElse(b).orElse {
        |  ZIO.fail(new RuntimeException("ooof"))
        |}""".stripMargin
    }
    val result = z {
      """b.orElse(b).orElseFail(new RuntimeException("ooof"))"""
    }
    testQuickFixes(text, result, hint)
  }

  def testMultiLineBlockHighlighting(): Unit =
    z {
      s"""b.orElse(b).${START}orElse {
         |  val a = 1
         |  val b = 2
         |  ZIO.fail(new RuntimeException("ooof"))
         |}$END""".stripMargin
    }.assertHighlighted()

  def testMultiLineBlockReplacement(): Unit = {
    val text = z {
      """b.orElse(b).orElse {
        |  val a = 1
        |  val b = 2
        |  ZIO.fail(new RuntimeException("ooof"))
        |}""".stripMargin
    }
    val result = z {
      """b.orElse(b).orElseFail {
        |  val a = 1
        |  val b = 2
        |  new RuntimeException("ooof")
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testNoHighlighting(): Unit =
    z {
      s"""b.orElse(b).${START}orElse {
         |  b *>
         |  ZIO.fail(new RuntimeException("ooof"))
         |}$END""".stripMargin
    }.assertNotHighlighted()
}
