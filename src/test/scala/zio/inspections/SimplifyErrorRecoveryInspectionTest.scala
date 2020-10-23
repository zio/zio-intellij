package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyErrorRecoveryInspection

abstract class SimplifyErrorRecoveryInspectionTest(toReplace: String, toReplaceWith: String, requiredParams: Int = 0)
    extends ZSimplifyInspectionTest[SimplifyErrorRecoveryInspection] {
  private val params = if (requiredParams == 0) "" else List.fill(requiredParams)("???").mkString("(", ", ", ")")

  private val methodToReplace     = s"$toReplace$params"
  private val methodToReplaceWith = s"map($toReplaceWith)"

  override protected val hint = s"Replace with .$methodToReplaceWith"

  def testInlineHighlighting(): Unit =
    z(s"${START}UIO(1).$methodToReplace$END").assertHighlighted()

  def testInlineReplacement(): Unit = {
    val text   = z(s"UIO(1).$methodToReplace")
    val result = z(s"UIO(1).$methodToReplaceWith")
    testQuickFixes(text, result, hint)
  }

  def testValHighlighting(): Unit = z {
    s"""val foo = UIO(1)
       |${START}foo.$methodToReplace$END""".stripMargin
  }.assertHighlighted()

  def testValReplacement(): Unit = {
    val text = z {
      s"""val foo = UIO(1)
         |foo.$methodToReplace""".stripMargin
    }
    val result = z {
      s"""val foo = UIO(1)
         |foo.$methodToReplaceWith""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testDefHighlighting(): Unit = z {
    s"""def foo = UIO(1)
       |${START}foo.$methodToReplace$END""".stripMargin
  }.assertHighlighted()

  def testDefReplacement(): Unit = {
    val text = z {
      s"""def foo = UIO(1)
         |foo.$methodToReplace""".stripMargin
    }
    val result = z {
      s"""def foo = UIO(1)
         |foo.$methodToReplaceWith""".stripMargin
    }
    testQuickFixes(text, result, hint)
  }

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"${START}Task(1).$methodToReplace$END").assertNotHighlighted()

}

class SimplifyOptionErrorRecoveryInspectionTest extends SimplifyErrorRecoveryInspectionTest("option", "Some(_)")
class SimplifyEitherErrorRecoveryInspectionTest extends SimplifyErrorRecoveryInspectionTest("either", "Right(_)")
class SimplifyOrElseEitherErrorRecoveryInspectionTest
    extends SimplifyErrorRecoveryInspectionTest("orElseEither", "Left(_)", 1)
class SimplifyOrElseEitherAliasErrorRecoveryInspectionTest
    extends SimplifyErrorRecoveryInspectionTest("<+>", "Left(_)", 1)
class SimplifyRetryOrElseEitherErrorRecoveryInspectionTest
    extends SimplifyErrorRecoveryInspectionTest("retryOrElseEither", "Right(_)", 2)
