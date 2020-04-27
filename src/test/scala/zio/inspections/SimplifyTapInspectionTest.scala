package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{ SELECTION_END_TAG => END, SELECTION_START_TAG => START }
import zio.intellij.inspections.simplifications.SimplifyTapInspection

abstract class SimplifyTapInspectionTestBase(s: String) extends ZSimplifyInspectionTest[SimplifyTapInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyTapErrorInspectionTest extends SimplifyTapInspectionTestBase(".tapError") {

  def test_infix_zio_fail(): Unit = {
    z(s"ZIO.unit.${START}catchAll(ex => logError(ex) *> ZIO.fail(ex))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.catchAll(ex => logError(ex) *> ZIO.fail(ex))")
    val result = z(s"ZIO.unit.tapError(ex => logError(ex))")
    testQuickFix(text, result, hint)
  }
}

class SimplifyTapInspectionTest extends SimplifyTapInspectionTestBase(".tap") {

  def foo(a: String => Unit) = ???

  def test_flatmap_reduce_func_no_params(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => f(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => f(a).as(a))")
    val result = z(s"ZIO.unit.tap(f)")
    testQuickFix(text, result, hint)
  }

  def test_flatmap_func_with_params(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => f(a, 42).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => f(a, 42).as(a))")
    val result = z(s"ZIO.unit.tap(a => f(a, 42))")
    testQuickFix(text, result, hint)
  }

  def test_flatmap_method_invocation_on_ref(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => logger.log(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => logger.log(a).as(a))")
    val result = z(s"ZIO.unit.tap(a => logger.log(a))")
    testQuickFix(text, result, hint)
  }

  def test_flatmap_to_value_underscore(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => b.as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => b.as(a))")
    val result = z(s"ZIO.unit.tap(_ => b)")
    testQuickFix(text, result, hint)
  }

  def test_flatmap_to_qualified_value_underscore(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => ZIO.unit.as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => ZIO.unit.as(a))")
    val result = z(s"ZIO.unit.tap(_ => ZIO.unit)")
    testQuickFix(text, result, hint)
  }
}
