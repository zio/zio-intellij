package zio.macros

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.junit.Assert._

class ModulePatternAccessiblePolyTest extends MacrosTest {

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader(zioOrg %% "zio-streams" % zioVersion, zioOrg %% "zio-macros" % zioVersion)

  override protected val code =
    s"""import zio._
       |import zio.blocking.Blocking
       |import zio.macros.accessible
       |import zio.stream.{ZSink, ZStream}
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
       |
       |    def f7(t: T): Managed[String, T]
       |    def f8(t: T): ZSink[R, String, T, T, List[T]]
       |    def f9(t: T): ZStream[Any, String, T]
       |  }
       |}
       |""".stripMargin

  def test_generates_accessor_function_for_field_of_the_polymorphic_service(): Unit =
    assertEquals(
      "def v[R, T <: Example.Foo]: zio.ZIO[zio.Has[Example.Service[R, T]] with R, Throwable, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].v)",
      method("v").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_without_argument_lists(): Unit =
    assertEquals(
      "def f1[R, T <: Example.Foo]: zio.ZIO[zio.Has[Example.Service[R, T]], Nothing, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].f1)",
      method("f1").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_empty_argument_list(): Unit =
    assertEquals(
      "def f2[R, T <: Example.Foo](): zio.ZIO[zio.Has[Example.Service[R, T]], Nothing, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].f2())",
      method("f2").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_single_argument(): Unit =
    assertEquals(
      "def f3[R, T <: Example.Foo](t: T): zio.ZIO[zio.Has[Example.Service[R, T]], Nothing, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].f3(t))",
      method("f3").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_multiple_argument_lists(): Unit =
    assertEquals(
      "def f4[R, T <: Example.Foo](t1: T)(t2: T): zio.ZIO[zio.Has[Example.Service[R, T]], Nothing, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].f4(t1)(t2))",
      method("f4").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_multiple_argument_lists_including_implicits()
    : Unit =
    assertEquals(
      "def f5[R, T <: Example.Foo](t1: T)(implicit t2: T): zio.ZIO[zio.Has[Example.Service[R, T]], Nothing, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].f5(t1)(t2))",
      method("f5").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_varargs(): Unit =
    assertEquals(
      "def f6[R, T <: Example.Foo](t: T*): zio.ZIO[zio.Has[Example.Service[R, T]], Nothing, T] = " +
        "zio.ZIO.accessM(_.get[Example.Service[R, T]].f6(t: _*))",
      method("f6").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_returning_managed(): Unit =
    assertEquals(
      "def f7[R, T <: Example.Foo](t: T): zio.ZManaged[zio.Has[Example.Service[R, T]], String, T] = " +
        "zio.ZManaged.accessManaged(_.get[Example.Service[R, T]].f7(t))",
      method("f7").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_returning_sink(): Unit =
    assertEquals(
      "def f8[R, T <: Example.Foo](t: T): zio.stream.ZSink[zio.Has[Example.Service[R, T]] with R, String, T, T, List[T]] = " +
        "zio.stream.ZSink.accessSink(_.get[Example.Service[R, T]].f8(t))",
      method("f8").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_returning_stream(): Unit =
    assertEquals(
      "def f9[R, T <: Example.Foo](t: T): zio.stream.ZStream[zio.Has[Example.Service[R, T]], String, T] = " +
        "zio.stream.ZStream.accessStream(_.get[Example.Service[R, T]].f9(t))",
      method("f9").getText
    )

}
