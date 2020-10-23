package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyErrorModificationInspection

abstract class SimplifyErrorModificationInspectionTest(toReplace: String, toReplaceWith: String)
    extends ZSimplifyInspectionTest[SimplifyErrorModificationInspection] {

  private val methodToReplace     = s"$toReplace(_ => ???, f)"
  private val methodToReplaceWith = s"$toReplaceWith(f)"

  override protected val hint = s"Replace with .$toReplaceWith"

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

class SimplifyBimapErrorModificationInspectionTest   extends SimplifyErrorModificationInspectionTest("bimap", "map")
class SimplifyTapBothErrorModificationInspectionTest extends SimplifyErrorModificationInspectionTest("tapBoth", "tap")
class SimplifyFoldErrorModificationInspectionTest    extends SimplifyErrorModificationInspectionTest("fold", "map")
class SimplifyFoldMErrorModificationInspectionTest   extends SimplifyErrorModificationInspectionTest("foldM", "flatMap")
class SimplifyFoldTraceMErrorModificationInspectionTest
    extends SimplifyErrorModificationInspectionTest("foldTraceM", "flatMap")
