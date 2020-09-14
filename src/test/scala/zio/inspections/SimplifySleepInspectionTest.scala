package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifySleepInspection

class SimplifySleepInspectionTest extends ZSimplifyInspectionTest[SimplifySleepInspection] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint = "Replace with .delay"

  def test_zipRight(): Unit = {
    z(s"""${START}ZIO.sleep(1.seconds) *> putStrLn("")$END""").assertHighlighted()
    val text   = z(s"""ZIO.sleep(1.seconds) *> putStrLn("")""")
    val result = z(s"""putStrLn("").delay(1.seconds)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_zipRight(): Unit = {
    z {
      s"""${START}ZIO.sleep {
         |  1.seconds
         |  1.seconds
         |  1.seconds
         |} *> putStrLn("")$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      s"""ZIO.sleep {
         |  1.seconds
         |  1.seconds
         |  1.seconds
         |} *> putStrLn("")""".stripMargin
    }
    val result = z {
      s"""putStrLn("").delay {
         |  1.seconds
         |  1.seconds
         |  1.seconds
         |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def test_flatMap(): Unit = {
    z(s"""${START}ZIO.sleep(1.seconds).flatMap(_ => putStrLn(""))$END""").assertHighlighted()
    val text   = z(s"""ZIO.sleep(1.seconds).flatMap(_ => putStrLn(""))""")
    val result = z(s"""putStrLn("").delay(1.seconds)""")
    testQuickFixes(text, result, hint)
  }

  def test_block_flatMap(): Unit = {
    z {
      s"""${START}ZIO.sleep {
         |  1.seconds
         |  1.seconds
         |  1.seconds
         |}.flatMap(_ => putStrLn(""))$END""".stripMargin
    }.assertHighlighted()
    val text = z {
      s"""ZIO.sleep {
         |  1.seconds
         |  1.seconds
         |  1.seconds
         |}.flatMap(_ => putStrLn(""))""".stripMargin
    }
    val result = z {
      s"""putStrLn("").delay {
         |  1.seconds
         |  1.seconds
         |  1.seconds
         |}""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }
}
