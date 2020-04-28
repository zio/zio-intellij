package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.mistakes.WrapInsteadOfLiftInspection

abstract class BaseWrapInsteadOfLiftInspectionTest(s: String)
    extends ZScalaInspectionTest[WrapInsteadOfLiftInspection] {
  override protected val description = WrapInsteadOfLiftInspection.messageFormat.format(s, s)
}

class FutureWrapInspectionTest extends BaseWrapInsteadOfLiftInspectionTest("Future") {

  val hint = "Replace with ZIO.fromFuture"

  def test_future_reference_ctor(): Unit = {
    z(s"""val future = Future(42)
         |${START}ZIO(future)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val future = Future(42)
                    |ZIO(future)""".stripMargin)
    val result = z(s"""val future = Future(42)
                      |ZIO.fromFuture(implicit ec => future)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_future_reference_apply(): Unit = {
    z(s"""val future = Future(42)
         |${START}ZIO.apply(future)$END""".stripMargin).assertHighlighted()
    val text   = z(s"""val future = Future(42)
                      |ZIO.apply(future)""".stripMargin)
    val result = z(s"""val future = Future(42)
                      |ZIO.fromFuture(implicit ec => future)""".stripMargin)
    testQuickFix(text, result, hint)
  }

  def test_future_direct_method(): Unit = {
    z(s"${START}ZIO(Future(42))$END").assertHighlighted()
    val text   = z(s"ZIO(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }

  def test_future_effect(): Unit = {
    z(s"${START}ZIO.effect(Future(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effect(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }

  def test_future_effectTotal(): Unit = {
    z(s"${START}ZIO.effectTotal(Future(42))$END").assertHighlighted()
    val text   = z(s"ZIO.effectTotal(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }

  def test_zio_alias_task(): Unit = {
    z(s"${START}Task.effectTotal(Future(42))$END").assertHighlighted()
    val text   = z(s"Task.effectTotal(Future(42))")
    val result = z(s"ZIO.fromFuture(implicit ec => Future(42))")
    testQuickFix(text, result, hint)
  }
}
