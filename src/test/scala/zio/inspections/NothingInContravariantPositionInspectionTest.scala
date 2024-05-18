package zio.inspections

import zio.intellij.inspections.mistakes.NothingInContravariantPositionInspection

class NothingInContravariantPositionInspectionTest
    extends ZScalaInspectionTest[NothingInContravariantPositionInspection] {

  override protected val description = NothingInContravariantPositionInspection.message

  def test_function_def(): Unit =
    z(s"def foo: ZIO[${START}Nothing$END, Nothing, Unit] = ???").assertHighlighted()

  def test_value_def(): Unit =
    z(s"val foo: ZIO[${START}Nothing$END, Nothing, Unit] = ???").assertHighlighted()

  def test_zlayer_def(): Unit =
    z(s"val foo: ZLayer[${START}Nothing$END, Nothing, Unit] = ???").assertHighlighted()

  def test_type_alias(): Unit =
    z(s"type Test[+A] = ZIO[${START}Nothing$END, Nothing, A] = ???").assertHighlighted()

  def test_does_not_highlight_usage(): Unit =
    z(s"def foo(z: ZIO[${START}Nothing$END, Nothing, Unit]) = ???").assertNotHighlighted()

  def test_highlights_Nothing_in_R(): Unit =
    z(s"type Dequeue[+A] = ZQueue[${START}Nothing$END, Nothing, Any, Nothing, Nothing, A]").assertHighlighted()

  def test_does_not_highlight_non_R_contravariant_positions(): Unit =
    z(s"type Dequeue[+A] = ZQueue[Any, Nothing, Any, Nothing, ${START}Nothing$END, A]").assertNotHighlighted()
}
