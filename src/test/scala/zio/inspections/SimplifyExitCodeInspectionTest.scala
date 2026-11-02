package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyExitCodeInspection

abstract class SimplifyExitCodeInspectionTest extends ZSimplifyInspectionTest[SimplifyExitCodeInspection] {

  override protected val hint = "Replace with .exitCode"

  def testFoldToExitCode(): Unit = {
    z(s"ZIO.succeed(42).${START}fold(_ => ExitCode.failure, _ => ExitCode.success)$END").assertHighlighted()
    testQuickFix(
      z("ZIO.succeed(42).fold(_ => ExitCode.failure, _ => ExitCode.success)"),
      z("ZIO.succeed(42).exitCode"),
      hint
    )
  }

  def testReversedExitCodesAreNotSimplified(): Unit =
    z("ZIO.succeed(42).fold(_ => ExitCode.success, _ => ExitCode.failure)").assertNotHighlighted()
}

class SimplifyExitCodeInspectionZIO1Test extends SimplifyExitCodeInspectionTest

class SimplifyExitCodeInspectionZIO2Test extends SimplifyExitCodeInspectionTest {
  override protected def isZIO1: Boolean = false
}
