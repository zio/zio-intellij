package zio.macros

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import org.junit.Assert._

class ModulePatternAccessiblePolyTest extends MacrosTest {

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader(zioOrg %% "zio-macros" % zioVersion)

  override protected val code =
    s"""import zio._
       |import zio.blocking.Blocking
       |import zio.macros.accessible
       |
       |@accessible
       |object E${CARET}xample {
       |  sealed trait Foo
       |
       |  trait Service[-R, T <: Foo] {
       |    val v: RIO[R, T]
       |
       |    def f1: UIO[T]
       |    def f2(): UIO[T]
       |    def f3(t: T): UIO[T]
       |    def f4(t1: T)(t2: T): UIO[T]
       |    def f5(t1: T)(implicit t2: T): UIO[T]
       |    def f6(t: T*): UIO[T]
       |  }
       |}
       |""".stripMargin

  def test_generates_accessor_function_for_field_of_the_polymorphic_service(): Unit = {
    assertEquals(
      "def v[R, T <: Example.Foo] = zio.ZIO.service[Example.Service[R, T]].flatMap(_.v)",
      method("v").getText
    )
    assertEquals(
      Right("ZIO[R with Has[Example.Service[R, T]], Throwable, T]"),
      method("v").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_without_argument_lists(): Unit = {
    assertEquals(
      "def f1[R, T <: Example.Foo] = zio.ZIO.service[Example.Service[R, T]].flatMap(_.f1)",
      method("f1").getText
    )
    assertEquals(
      Right("ZIO[Has[Example.Service[R, T]], Nothing, T]"),
      method("f1").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_empty_argument_list(): Unit = {
    assertEquals(
      "def f2[R, T <: Example.Foo]() = zio.ZIO.service[Example.Service[R, T]].flatMap(_.f2())",
      method("f2").getText
    )
    assertEquals(
      Right("() => ZIO[Has[Example.Service[R, T]], Nothing, T]"),
      method("f2").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_single_argument(): Unit = {
    assertEquals(
      "def f3[R, T <: Example.Foo](t: T) = zio.ZIO.service[Example.Service[R, T]].flatMap(_.f3(t))",
      method("f3").getText
    )
    assertEquals(
      Right("T => ZIO[Has[Example.Service[R, T]], Nothing, T]"),
      method("f3").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_multiple_argument_lists(): Unit = {
    assertEquals(
      "def f4[R, T <: Example.Foo](t1: T)(t2: T) = zio.ZIO.service[Example.Service[R, T]].flatMap(_.f4(t1)(t2))",
      method("f4").getText
    )
    assertEquals(
      Right("T => T => ZIO[Has[Example.Service[R, T]], Nothing, T]"),
      method("f4").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_multiple_argument_lists_including_implicits()
    : Unit = {
    assertEquals(
      "def f5[R, T <: Example.Foo](t1: T)(implicit t2: T) = zio.ZIO.service[Example.Service[R, T]].flatMap(_.f5(t1)(t2))",
      method("f5").getText
    )
    assertEquals(
      Right("T => T => ZIO[Has[Example.Service[R, T]], Nothing, T]"),
      method("f5").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_varargs(): Unit =
    assertEquals(
      "def f6[R, T <: Example.Foo](t: T*) = zio.ZIO.service[Example.Service[R, T]].flatMap(_.f6(t: _*))",
      method("f6").getText
    )

}
