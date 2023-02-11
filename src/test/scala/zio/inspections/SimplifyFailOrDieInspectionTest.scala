package zio.inspections

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
    testQuickFix(text, result, hint)
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
    testQuickFix(text, result, hint)
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
    testQuickFix(text, result, hint)
  }
}
