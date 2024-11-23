package zio.inspections

import zio.intellij.inspections.mistakes.ZIOFailThrowInspection

class ZIOFailThrowInspectionTest extends ZScalaInspectionTest[ZIOFailThrowInspection] {

  override protected def description: String = ZIOFailThrowInspection.message
  val hint = "Remove throw from ZIO.fail"

  def test_throw_inside_zio_fail(): Unit = {
    z(s"""for {
         |  x <- ${START}ZIO.fail(throw new RuntimeException("FAIL"))${END}
         |} yield ()""".stripMargin).assertHighlighted()
    val text   = z(s"""for {
                      |  x <- ZIO.fail(throw new RuntimeException("FAIL"))
                      |} yield ()""".stripMargin)
    val result = z(s"""for {
                      |  x <- ZIO.fail(new RuntimeException("FAIL"))
                      |} yield ()""".stripMargin)
    testQuickFix(text, result, hint)
  }
}
