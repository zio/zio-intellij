package zio.macros

import org.junit.Assert._

class ModulePatternAccessibleWrongAliasTest extends MacrosTest {

  override protected val code =
    s"""
import zio._
import zio.blocking.Blocking
import zio.macros.accessible

@accessible
object E${CARET}xample {
  type Environment = Blocking

  type EIO[+T] = ZIO[Environment, Nothing, T]

  type Example = Has[Int]

  trait Service {
    val v: EIO[Boolean]
  }
}
"""

  def test_generates_accessor_without_alias(): Unit =
    assertEquals(
      "val v: zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Boolean] = _root_.scala.Predef.???",
      field("v").getText
    )
}
