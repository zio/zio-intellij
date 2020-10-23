package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.mistakes.InfallibleEffectRecoveryInspection

abstract class InfallibleEffectRecoveryInspectionTest(fromEffect: String => String)
    extends ZScalaInspectionTest[InfallibleEffectRecoveryInspection] {
  private val faultyMethod = "orElse"

  override protected val description = InfallibleEffectRecoveryInspection.description(faultyMethod)
  private val hint                   = InfallibleEffectRecoveryInspection.hint(faultyMethod)

  def testInlineHighlighting(): Unit =
    z(s"$START${fromEffect("UIO(1)")}.$faultyMethod(???)$END").assertHighlighted()

  def testInlineReplacement(): Unit = {
    val text   = z(s"${fromEffect("UIO(1)")}.$faultyMethod(???)")
    val result = z(fromEffect("UIO(1)"))
    testQuickFixes(text, result, hint)
  }

  def testValHighlighting(): Unit = z {
    s"""val foo = ${fromEffect("UIO(1)")}
       |${START}foo.$faultyMethod(???)$END""".stripMargin
  }.assertHighlighted()

  def testValReplacement(): Unit = {
    val text = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod(???)""".stripMargin
    }
    val result = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testDefHighlighting(): Unit = z {
    s"""def foo = ${fromEffect("UIO(1)")}
       |${START}foo.$faultyMethod(???)$END""".stripMargin
  }.assertHighlighted()

  def testDefReplacement(): Unit = {
    val text = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod(???)""".stripMargin
    }
    val result = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testInlineBlockHighlighting(): Unit =
    z {
      s"""$START${fromEffect("UIO(1)")}.$faultyMethod {
         |  val a = 1
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()

  def testInlineBlockReplacement(): Unit = {
    val text = z {
      s"""${fromEffect("UIO(1)")}.$faultyMethod {
         |  val a = 1
         |  ???
         |}""".stripMargin
    }
    val result = z(s"${fromEffect("UIO(1)")}")
    testQuickFixes(text, result, hint)
  }

  def testValBlockHighlighting(): Unit =
    z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |${START}foo.$faultyMethod {
         |  val a = 1
         |  ???
         |}$END""".stripMargin
    }.assertHighlighted()

  def testValBlockReplacement(): Unit = {
    val text = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod {
         |  val a = 1
         |  ???
         |}""".stripMargin
    }
    val result = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testDefBlockHighlighting(): Unit = z {
    s"""def foo = ${fromEffect("UIO(1)")}
       |${START}foo.$faultyMethod {
       |  val a = 1
       |  ???
       |}$END""".stripMargin
  }.assertHighlighted()

  def testDefBlockReplacement(): Unit = {
    val text = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod {
         |  val a = 1
         |  ???
         |}""".stripMargin
    }
    val result = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"$START${fromEffect("Task(1)")}.$faultyMethod(???)$END").assertNotHighlighted()
}

class InfallibleZIORecoveryInspectionTest extends InfallibleEffectRecoveryInspectionTest(identity)
class InfallibleZManagedRecoveryInspectionTest
    extends InfallibleEffectRecoveryInspectionTest(effect => s"ZManaged.fromEffect($effect)")
class InfallibleZStreamRecoveryInspectionTest
    extends InfallibleEffectRecoveryInspectionTest(effect => s"ZStream.fromEffect($effect)")
