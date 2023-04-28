package zio.macros

import org.junit.Assert._

abstract class ModulePatternAccessibleTestBase(injectAlias: Boolean) extends MacrosTest {

  val aliasOrHasService = if (injectAlias) "Example.Example" else "zio.Has[Example.Service]"

  override protected val code =
    s"""
import zio._
import zio.blocking.Blocking
import zio.macros.accessible
import zio.stream.{ZSink, ZStream}

@accessible
object E${CARET}xample {
  type Environment = Blocking

  type EIO[+T] = ZIO[Environment, Nothing, T]

  ${if (injectAlias) "type Example = Has[Service]" else ""}

  sealed trait Foo { val value: String }
  final case class Bar(value: String) extends Foo
  final case class Wrapped[T](value: T)

  trait Service {
    val v: EIO[Boolean]
    def m0: EIO[Unit]
    def m1(s: String): EIO[Int]
    def m2[T](s2: String = "")(p: (T, Int))(i2: Int*): UIO[Double]
    def m3[T <: Foo](t: Wrapped[T]): IO[String, List[T]]

    val defaultPureValue: EIO[Boolean] = v
    def defaultPureMethod(s: String): EIO[Int] = m1

    val vManaged: Managed[String, Foo]
    def mManaged(s: String): ZManaged[Any, Nothing, Bar]

    val vNonZIO: Boolean
    def m0NonZIO: Unit
    def m1NonZIO(s: String): Int
    def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*): Double
    def stream(n: Int): ZStream[Any, String, Int]
    def sink(n: Int): ZSink[Any, String, Int, Int, List[Int]]

    val defaultImpureValue: Boolean = vNonZIO
    def defaultImpureMethod(s: String): Int = m1NonZIO(s)
  }
}
"""

  def test_generates_accessor_value_for_ZIO_field(): Unit =
    assertEquals(
      s"val v: zio.ZIO[$aliasOrHasService with Example.Environment, Nothing, Boolean] = _root_.scala.Predef.???",
      field("v").getText
    )

  def test_generates_accessor_function_for_ZIO_method_without_arguments(): Unit =
    assertEquals(
      s"def m0: zio.ZIO[$aliasOrHasService with Example.Environment, Nothing, Unit] = _root_.scala.Predef.???",
      method("m0").getText
    )

  def test_generates_accessor_function_for_ZIO_method_with_argument(): Unit =
    assertEquals(
      s"def m1(s: String): zio.ZIO[$aliasOrHasService with Example.Environment, Nothing, Int] = _root_.scala.Predef.???",
      method("m1").getText
    )

  def test_generates_accessor_function_for_generic_ZIO_method_with_multiple_arg_lists_default_args_and_varargs(): Unit =
    assertEquals(
      s"""def m2[T](s2: String = "")(p: (T, Int))(i2: Int*): zio.ZIO[$aliasOrHasService, Nothing, Double] = _root_.scala.Predef.???""",
      method("m2").getText
    )

  def test_generates_accessor_function_for_generic_ZIO_method_with_type_constraints(): Unit =
    assertEquals(
      s"def m3[T <: Foo](t: Wrapped[T]): zio.ZIO[$aliasOrHasService, String, List[T]] = _root_.scala.Predef.???",
      method("m3").getText
    )

  def test_generates_accessor_value_for_ZIO_field_with_default_implementation(): Unit =
    assertEquals(
      s"val defaultPureValue: zio.ZIO[$aliasOrHasService with Example.Environment, Nothing, Boolean] = _root_.scala.Predef.???",
      field("defaultPureValue").getText
    )

  def test_generates_accessor_function_for_ZIO_method_with_argument_with_default_implementation(): Unit =
    assertEquals(
      s"def defaultPureMethod(s: String): zio.ZIO[$aliasOrHasService with Example.Environment, Nothing, Int] = _root_.scala.Predef.???",
      method("defaultPureMethod").getText
    )

  def test_generates_accessor_value_for_non_ZIO_field(): Unit =
    assertEquals(
      s"val vNonZIO: zio.ZIO[$aliasOrHasService, Throwable, Boolean] = _root_.scala.Predef.???",
      field("vNonZIO").getText
    )

  def test_generates_accessor_function_for_non_ZIO_method_without_arguments(): Unit =
    assertEquals(
      s"def m0NonZIO: zio.ZIO[$aliasOrHasService, Throwable, Unit] = _root_.scala.Predef.???",
      method("m0NonZIO").getText
    )

  def test_generates_accessor_function_for_non_ZIO_method_with_argument(): Unit =
    assertEquals(
      s"def m1NonZIO(s: String): zio.ZIO[$aliasOrHasService, Throwable, Int] = _root_.scala.Predef.???",
      method("m1NonZIO").getText
    )

  def test_generates_accessor_function_for_generic_non_ZIO_method_with_multiple_arg_lists_default_args_and_varargs()
    : Unit =
    assertEquals(
      s"""def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*): zio.ZIO[$aliasOrHasService, Throwable, Double] = _root_.scala.Predef.???""",
      method("m2NonZIO").getText
    )

  def test_generates_accessor_function_for_ZIO_method_returning_stream(): Unit =
    assertEquals(
      s"def stream(n: Int): zio.stream.ZStream[$aliasOrHasService, String, Int] = _root_.scala.Predef.???",
      method("stream").getText
    )

  def test_generates_accessor_function_for_ZIO_method_returning_sink(): Unit =
    assertEquals(
      s"def sink(n: Int): zio.stream.ZSink[$aliasOrHasService, String, Int, Int, List[Int]] = _root_.scala.Predef.???",
      method("sink").getText
    )

  def test_generates_accessor_value_for_non_ZIO_field_with_default_implementation(): Unit =
    assertEquals(
      s"val defaultImpureValue: zio.ZIO[$aliasOrHasService, Throwable, Boolean] = _root_.scala.Predef.???",
      field("defaultImpureValue").getText
    )

  def test_generates_accessor_function_for_non_ZIO_method_with_argument_with_default_implementation(): Unit =
    assertEquals(
      s"def defaultImpureMethod(s: String): zio.ZIO[$aliasOrHasService, Throwable, Int] = _root_.scala.Predef.???",
      method("defaultImpureMethod").getText
    )

  def test_generates_accessor_value_for_managed_field(): Unit =
    assertEquals(
      s"val vManaged: zio.ZManaged[$aliasOrHasService, String, Example.Foo] = _root_.scala.Predef.???",
      field("vManaged").getText
    )

  def test_generates_accessor_function_for_method_returning_managed(): Unit =
    assertEquals(
      s"def mManaged(s: String): zio.ZManaged[$aliasOrHasService, Nothing, Example.Bar] = _root_.scala.Predef.???",
      method("mManaged").getText
    )

}

class ModulePatternAccessibleTest      extends ModulePatternAccessibleTestBase(injectAlias = false)
class ModulePatternAccessibleAliasTest extends ModulePatternAccessibleTestBase(injectAlias = true)

class ModulePatternAccessibleZIO2Test extends MacrosTest {

  override protected val isZIO1: Boolean = false

  override protected val code =
    s"""import zio._
       |import zio.macros.accessible
       |
       |@accessible
       |trait Example {
       |  def m0(s: String): URIO[Int, Unit]
       |}
       |
       |object E${CARET}xample""".stripMargin

  // all of the required check are made above
  // here we just check that it works fine for ZIO2 and doesn't use Has[_]
  def test_generates_accessor_function_for_ZIO2_method(): Unit =
    assertEquals(
      s"def m0(s: String): zio.ZIO[Example with Int, Nothing, Unit] = _root_.scala.Predef.???",
      method("m0").getText
    )

}
