package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyIgnoreInspection

abstract class SimplifyIgnoreInspectionTest extends ZSimplifyInspectionTest[SimplifyIgnoreInspection] {

  private val foldCauseM = if (isZIO1) "foldCauseM" else "foldCauseZIO"
  private val Task       = if (isZIO1) "Task" else "ZIO.attempt"

  override protected val hint = "Replace with .ignore"

  def test_catchAll_ZIOUnit(): Unit = {
    z(s"ZIO.succeed(42).${START}catchAll(_ => ZIO.unit)$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).catchAll(_ => ZIO.unit)")
    val result = z("ZIO.succeed(42).ignore")
    testQuickFix(text, result, hint)
  }

  def test_foldCause_unit(): Unit = {
    z(s"ZIO.succeed(42).${START}foldCause(_ => (), _ => ())$END").assertHighlighted()
    val text   = z("ZIO.succeed(42).foldCause(_ => (), _ => ())")
    val result = z("ZIO.succeed(42).ignore")
    testQuickFix(text, result, hint)
  }

  def test_foldCauseM_ZIOUnit(): Unit = {
    z(s"ZIO.succeed(42).$START$foldCauseM(_ => ZIO.unit, _ => ZIO.unit)$END").assertHighlighted()
    val text   = z(s"ZIO.succeed(42).$foldCauseM(_ => ZIO.unit, _ => ZIO.unit)")
    val result = z("ZIO.succeed(42).ignore")
    testQuickFix(text, result, hint)
  }

  def test_either_unit_ignore(): Unit = {
    z(s"$Task(42).${START}either.unit$END").assertHighlighted()
    val text   = z(s"$Task(42).either.unit")
    val result = z(s"$Task(42).ignore")
    testQuickFix(text, result, hint)
  }
}

class SimplifyIgnoreInspectionZIO1Test extends SimplifyIgnoreInspectionTest
class SimplifyIgnoreInspectionZIO2Test extends SimplifyIgnoreInspectionTest {
  override def isZIO1: Boolean = false
}
