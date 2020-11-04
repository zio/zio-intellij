package zio.macros

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.junit.Assert._

class ModulePatternAccessibleWrongAliasTest extends MacrosTest {

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader(zioOrg %% "zio-streams" % zioVersion, zioOrg %% "zio-macros" % zioVersion)

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
      "val v: zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Boolean] = " +
        "zio.ZIO.accessM(_.get[Example.Service].v)",
      field("v").getText
    )
}
