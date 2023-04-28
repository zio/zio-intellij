package zio.macros

import org.junit.Assert._

abstract class ModulePatternAccessiblePolyTestBase(injectAlias: Boolean) extends MacrosTest {

  val aliasOrHasService = if (injectAlias) "Example.Example[R, T]" else "zio.Has[Example.Service[R, T]]"

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
       |  ${if (injectAlias) "type Example[-R, T <: Foo] = Has[Service[R, T]]" else ""}
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
      s"def v[R, T <: Foo]: zio.ZIO[$aliasOrHasService with R, Throwable, T] = _root_.scala.Predef.???",
      method("v").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_without_argument_lists(): Unit =
    assertEquals(
      s"def f1[R, T <: Foo]: zio.ZIO[$aliasOrHasService, Nothing, T] = _root_.scala.Predef.???",
      method("f1").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_empty_argument_list(): Unit =
    assertEquals(
      s"def f2[R, T <: Foo](): zio.ZIO[$aliasOrHasService, Nothing, T] = _root_.scala.Predef.???",
      method("f2").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_single_argument(): Unit =
    assertEquals(
      s"def f3[R, T <: Foo](t: T): zio.ZIO[$aliasOrHasService, Nothing, T] = _root_.scala.Predef.???",
      method("f3").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_multiple_argument_lists(): Unit =
    assertEquals(
      s"def f4[R, T <: Foo](t1: T)(t2: T): zio.ZIO[$aliasOrHasService, Nothing, T] = _root_.scala.Predef.???",
      method("f4").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_multiple_argument_lists_including_implicits()
    : Unit =
    assertEquals(
      s"def f5[R, T <: Foo](t1: T)(implicit t2: T): zio.ZIO[$aliasOrHasService, Nothing, T] = _root_.scala.Predef.???",
      method("f5").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_with_varargs(): Unit =
    assertEquals(
      s"def f6[R, T <: Foo](t: T*): zio.ZIO[$aliasOrHasService, Nothing, T] = _root_.scala.Predef.???",
      method("f6").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_returning_managed(): Unit =
    assertEquals(
      s"def f7[R, T <: Foo](t: T): zio.ZManaged[$aliasOrHasService, String, T] = _root_.scala.Predef.???",
      method("f7").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_returning_sink(): Unit =
    assertEquals(
      s"def f8[R, T <: Foo](t: T): zio.stream.ZSink[$aliasOrHasService with R, String, T, T, List[T]] = _root_.scala.Predef.???",
      method("f8").getText
    )

  def test_generates_accessor_function_for_method_of_the_polymorphic_service_returning_stream(): Unit =
    assertEquals(
      s"def f9[R, T <: Foo](t: T): zio.stream.ZStream[$aliasOrHasService, String, T] = _root_.scala.Predef.???",
      method("f9").getText
    )

}

class ModulePatternAccessiblePolyTest      extends ModulePatternAccessiblePolyTestBase(injectAlias = false)
class ModulePatternAccessiblePolyAliasTest extends ModulePatternAccessiblePolyTestBase(injectAlias = true)
