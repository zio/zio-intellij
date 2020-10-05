package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyFailInspection

class SimplifyFailOrDieInspectionTest extends ZSimplifyInspectionTest[SimplifyFailInspection] {

  override protected val hint = s"Replace with ZIO.die"

  def testOneLineHighlighting(): Unit =
    z {
      s"""${START}ZIO.fail(new RuntimeException("ooof")).orDie$END"""
    }.assertHighlighted()

  def testOneLineReplacement(): Unit = {
    val text = z {
      """ZIO.fail(new RuntimeException("ooof")).orDie"""
    }
    val result = z {
      """ZIO.die(new RuntimeException("ooof"))"""
    }
    testQuickFixes(text, result, hint)
  }

  def testOneLineBlockHighlighting(): Unit =
    z {
      s"""${START}ZIO.fail {
         |  new RuntimeException("ooof")
         |}.orDie$END""".stripMargin
    }.assertHighlighted()

  def testOneLineBlockReplacement(): Unit = {
    val text = z {
      """ZIO.fail {
        |  new RuntimeException("ooof")
        |}.orDie""".stripMargin
    }
    val result = z {
      """ZIO.die(new RuntimeException("ooof"))"""
    }
    testQuickFixes(text, result, hint)
  }

  def testMultiLineBlockHighlighting(): Unit =
    z {
      s"""${START}ZIO.fail {
         |  val a = 1
         |  val b = 2
         |  new RuntimeException("ooof")
         |}.orDie$END""".stripMargin
    }.assertHighlighted()

  def testMultiLineBlockReplacement(): Unit = {
    val text = z {
      """ZIO.fail {
        |  val a = 1
        |  val b = 2
        |  new RuntimeException("ooof")
        |}.orDie""".stripMargin
    }
    val result = z {
      """ZIO.die {
        |  val a = 1
        |  val b = 2
        |  new RuntimeException("ooof")
        |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }
}
