package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.mistakes.InfallibleEffectRecoveryInspection

class InfallibleEffectRecoveryInspectionTest extends ZScalaInspectionTest[InfallibleEffectRecoveryInspection] {
  private val faultyMethod = "orElse"

  override protected val description = InfallibleEffectRecoveryInspection.description(faultyMethod)
  private val hint                   = InfallibleEffectRecoveryInspection.hint(faultyMethod)

  def testInlineHighlighting(): Unit =
    z(s"${START}UIO(1).$faultyMethod(b)$END").assertHighlighted()

  def testInlineReplacement(): Unit = {
    val text   = z(s"UIO(1).$faultyMethod(b)")
    val result = z("UIO(1)")
    testQuickFixes(text, result, hint)
  }

  def testValHighlighting(): Unit = z {
    s"""val foo = UIO(1)
       |${START}foo.$faultyMethod(b)$END""".stripMargin
  }.assertHighlighted()

  def testValReplacement(): Unit = {
    val text = z {
      s"""val foo = UIO(1)
         |foo.$faultyMethod(b)""".stripMargin
    }
    val result = z {
      """val foo = UIO(1)
        |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testDefHighlighting(): Unit = z {
    s"""def foo = UIO(1)
       |${START}foo.$faultyMethod(b)$END""".stripMargin
  }.assertHighlighted()

  def testDefReplacement(): Unit = {
    val text = z {
      s"""def foo = UIO(1)
         |foo.$faultyMethod(b)""".stripMargin
    }
    val result = z {
      """def foo = UIO(1)
        |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testInlineBlockHighlighting(): Unit =
    z {
      s"""${START}UIO(1).$faultyMethod {
         |  val a = 1
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()

  def testInlineBlockReplacement(): Unit = {
    val text = z {
      s"""UIO(1).$faultyMethod {
         |  val a = 1
         |  b
         |}""".stripMargin
    }
    val result = z("UIO(1)")
    testQuickFixes(text, result, hint)
  }

  def testValBlockHighlighting(): Unit =
    z {
      s"""val foo = UIO(1)
         |${START}foo.$faultyMethod {
         |  val a = 1
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()

  def testValBlockReplacement(): Unit = {
    val text = z {
      s"""val foo = UIO(1)
         |foo.$faultyMethod {
         |  val a = 1
         |  b
         |}""".stripMargin
    }
    val result = z {
      """val foo = UIO(1)
        |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testDefBlockHighlighting(): Unit = z {
    s"""def foo = UIO(1)
       |${START}foo.$faultyMethod {
       |  val a = 1
       |  b
       |}$END""".stripMargin
  }.assertHighlighted()

  def testDefBlockReplacement(): Unit = {
    val text = z {
      s"""def foo = UIO(1)
         |foo.$faultyMethod {
         |  val a = 1
         |  b
         |}""".stripMargin
    }
    val result = z {
      """def foo = UIO(1)
        |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"${START}Task(1).$faultyMethod(b)$END").assertNotHighlighted()
}
