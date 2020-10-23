package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyErrorSeparationInspection

abstract class SimplifyErrorSeparationInspectionTest(toReplace: String, toReplaceWith: String, isParN: Boolean = false)
    extends ZSimplifyInspectionTest[SimplifyErrorSeparationInspection] {

  private val nParamList: String = if (isParN) "(n)" else ""

  private def methodToReplace(f: String)     = s"ZIO.$toReplace$nParamList(List(1, 2, 3))($f)"
  private def methodToReplaceWith(f: String) = s"ZIO.$toReplaceWith$nParamList(List(1, 2, 3))($f)"

  override protected val hint = s"Replace with ZIO.$toReplaceWith"

  private def zdef(s: String): String = z {
    s"""
       |def foo(el: Any) = UIO(el)
       |$s
       |""".stripMargin
  }

  private def zval(s: String): String = z {
    s"""
       |val foo: Any => UIO[Any] = el => UIO(el)
       |$s
       |""".stripMargin
  }

  def testDefRefHighlighting(): Unit = zdef(s"$START${methodToReplace("foo")}$END").assertHighlighted()

  def testDefRefReplacement(): Unit = {
    val text   = zdef(s"${methodToReplace("foo")}")
    val result = zdef(s"${methodToReplaceWith("foo")}")
    testQuickFixes(text, result, hint)
  }

  def testDefUnderscoreHighlighting(): Unit = zdef(s"$START${methodToReplace("foo(_)")}$END").assertHighlighted()

  def testDefUnderscoreReplacement(): Unit = {
    val text   = zdef(s"${methodToReplace("foo(_)")}")
    val result = zdef(s"${methodToReplaceWith("foo(_)")}")
    testQuickFixes(text, result, hint)
  }

  def testDefLambdaHighlighting(): Unit = zdef(s"$START${methodToReplace("el => foo(el)")}$END").assertHighlighted()

  def testDefLambdaReplacement(): Unit = {
    val text   = zdef(s"${methodToReplace("el => foo(el)")}")
    val result = zdef(s"${methodToReplaceWith("el => foo(el)")}")
    testQuickFixes(text, result, hint)
  }

  def testValRefHighlighting(): Unit = zval(s"$START${methodToReplace("foo")}$END").assertHighlighted()

  def testValRefReplacement(): Unit = {
    val text   = zval(s"${methodToReplace("foo")}")
    val result = zval(s"${methodToReplaceWith("foo")}")
    testQuickFixes(text, result, hint)
  }

  def testValUnderscoreHighlighting(): Unit = zval(s"$START${methodToReplace("foo(_)")}$END").assertHighlighted()

  def testValUnderscoreReplacement(): Unit = {
    val text   = zval(s"${methodToReplace("foo(_)")}")
    val result = zval(s"${methodToReplaceWith("foo(_)")}")
    testQuickFixes(text, result, hint)
  }

  def testValLambdaHighlighting(): Unit = zval(s"$START${methodToReplace("el => foo(el)")}$END").assertHighlighted()

  def testValLambdaReplacement(): Unit = {
    val text   = zval(s"${methodToReplace("el => foo(el)")}")
    val result = zval(s"${methodToReplaceWith("el => foo(el)")}")
    testQuickFixes(text, result, hint)
  }

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"$START${methodToReplace("Task(_)")}$END").assertNotHighlighted()

}

class SimplifyPartitionErrorSeparationInspectionTest
    extends SimplifyErrorSeparationInspectionTest("partition", "foreach")
class SimplifyPartitionParErrorSeparationInspectionTest
    extends SimplifyErrorSeparationInspectionTest("partitionPar", "foreachPar")
class SimplifyPartitionParNErrorSeparationInspectionTest
    extends SimplifyErrorSeparationInspectionTest("partitionParN", "foreachParN", isParN = true)
class SimplifyValidateErrorSeparationInspectionTest extends SimplifyErrorSeparationInspectionTest("validate", "foreach")
class SimplifyValidateParErrorSeparationInspectionTest
    extends SimplifyErrorSeparationInspectionTest("validatePar", "foreachPar")
class SimplifyValidateFirstErrorSeparationInspectionTest
    extends SimplifyErrorSeparationInspectionTest("validateFirst", "foreach")
class SimplifyValidateFirstParErrorSeparationInspectionTest
    extends SimplifyErrorSeparationInspectionTest("validateFirstPar", "foreachPar")
