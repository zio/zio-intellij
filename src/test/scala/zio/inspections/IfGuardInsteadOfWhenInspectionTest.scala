package zio.inspections

import zio.intellij.inspections.mistakes.IfGuardInsteadOfWhenInspection

class IfGuardInsteadOfWhenInspectionTest extends ZScalaInspectionTest[IfGuardInsteadOfWhenInspection] {
  override protected val description = IfGuardInsteadOfWhenInspection.problemMessage
  private val hint                   = IfGuardInsteadOfWhenInspection.fixMessage

  def test_if_guards_on_discarded_zio_effect(): Unit = {
    z(s"""for {
         |  x <- ZIO.succeed(1) if 1 == 2
         |  _ <- ZIO.succeed(2) ${START}if 1 == 2$END // regular guard
         |  _ <- ZIO.succeed(3) /* guard after block comment */ ${START}if 1 == 2$END
         |  _ <- ZIO.succeed(4)
         |  _ <- ZIO.succeed(5) // guard on the new line after line comment
         |  ${START}if 2 == 2$END
         |  y <- ZIO.succeed(6)
         |} yield ()""".stripMargin).assertHighlighted()

    val text = z(s"""for {
                    |  x <- ZIO.succeed(1) if 1 == 2
                    |  _ <- ZIO.succeed(2) if 1 == 2 // regular guard
                    |  _ <- ZIO.succeed(3) /* guard after block comment */ if 1 == 2
                    |  _ <- ZIO.succeed(4)
                    |  _ <- ZIO.succeed(5) // guard on the new line after line comment
                    |  if 2 == 2
                    |  y <- ZIO.succeed(6)
                    |} yield ()""".stripMargin)

    val result = z(s"""for {
                      |  x <- ZIO.succeed(1) if 1 == 2
                      |  _ <- ZIO.succeed(2).when(1 == 2) // regular guard
                      |  _ <- ZIO.succeed(3).when(1 == 2) /* guard after block comment */
                      |  _ <- ZIO.succeed(4)
                      |  _ <- ZIO.succeed(5).when(2 == 2) // guard on the new line after line comment
                      |  y <- ZIO.succeed(6)
                      |} yield ()""".stripMargin)

    testQuickFixAllInFile(text, result, hint)
  }
}
