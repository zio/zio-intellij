package zio.macros

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import org.junit.Assert._

class ModulePatternAccessibleTest extends MacrosTest {

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader(zioOrg %% "zio-streams" % zioVersion, zioOrg %% "zio-macros" % zioVersion)

  override protected val code =
    s"""
import zio._
import zio.blocking.Blocking
import zio.macros.accessible
import zio.stream.ZStream

@accessible
object E${CARET}xample {
  type Environment = Blocking

  type EIO[+T] = ZIO[Environment, Nothing, T]

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

    val defaultImpureValue: Boolean = vNonZIO
    def defaultImpureMethod(s: String): Int = m1NonZIO(s)
  }
}
"""

  def test_generates_accessor_value_for_ZIO_field(): Unit = {
    assertEquals(
      "val v = zio.ZIO.service[Example.Service].flatMap(_.v)",
      field("v").getText
    )
    assertEquals(
      Right("ZIO[Example.Environment with Has[Example.Service], Nothing, Boolean]"),
      field("v").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_ZIO_method_without_arguments(): Unit = {
    assertEquals(
      "def m0 = zio.ZIO.service[Example.Service].flatMap(_.m0)",
      method("m0").getText
    )
    assertEquals(
      Right("ZIO[Example.Environment with Has[Example.Service], Nothing, Unit]"),
      method("m0").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_ZIO_method_with_argument(): Unit = {
    assertEquals(
      "def m1(s: String) = zio.ZIO.service[Example.Service].flatMap(_.m1(s))",
      method("m1").getText
    )
    assertEquals(
      Right("String => ZIO[Example.Environment with Has[Example.Service], Nothing, Int]"),
      method("m1").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_generic_ZIO_method_with_multiple_arg_lists_default_args_and_varargs()
    : Unit = {
    assertEquals(
      """def m2[T](s2: String = "")(p: (T, Int))(i2: Int*) = zio.ZIO.service[Example.Service].flatMap(_.m2[T](s2)(p)(i2: _*))""",
      method("m2").getText
    )
    assertEquals(
      Right("ZIO[Has[Example.Service], Nothing, Double]"),
      method("m2").returnType.map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_generic_ZIO_method_with_type_constraints(): Unit = {
    assertEquals(
      """def m3[T <: Example.Foo](t: Example.Wrapped[T]) = zio.ZIO.service[Example.Service].flatMap(_.m3[T](t))""",
      method("m3").getText
    )
    assertEquals(
      Right("Example.Wrapped[T] => ZIO[Has[Example.Service], String, List[T]]"),
      method("m3").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_value_for_ZIO_field_with_default_implementation(): Unit = {
    assertEquals(
      "val defaultPureValue = zio.ZIO.service[Example.Service].flatMap(_.defaultPureValue)",
      field("defaultPureValue").getText
    )
    assertEquals(
      Right("ZIO[Example.Environment with Has[Example.Service], Nothing, Boolean]"),
      field("defaultPureValue").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_ZIO_method_with_argument_with_default_implementation(): Unit = {
    assertEquals(
      "def defaultPureMethod(s: String) = zio.ZIO.service[Example.Service].flatMap(_.defaultPureMethod(s))",
      method("defaultPureMethod").getText
    )
    assertEquals(
      Right("String => ZIO[Example.Environment with Has[Example.Service], Nothing, Int]"),
      method("defaultPureMethod").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_value_for_non_ZIO_field(): Unit = {
    assertEquals(
      "val vNonZIO = zio.ZIO.service[Example.Service].map(_.vNonZIO)",
      field("vNonZIO").getText
    )
    assertEquals(
      Right("ZIO[Has[Example.Service], Nothing, Boolean]"),
      field("vNonZIO").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_non_ZIO_method_without_arguments(): Unit = {
    assertEquals(
      "def m0NonZIO = zio.ZIO.service[Example.Service].map(_.m0NonZIO)",
      method("m0NonZIO").getText
    )
    assertEquals(
      Right("ZIO[Has[Example.Service], Nothing, Unit]"),
      method("m0NonZIO").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_non_ZIO_method_with_argument(): Unit = {
    assertEquals(
      "def m1NonZIO(s: String) = zio.ZIO.service[Example.Service].map(_.m1NonZIO(s))",
      method("m1NonZIO").getText
    )
    assertEquals(
      Right("String => ZIO[Has[Example.Service], Nothing, Int]"),
      method("m1NonZIO").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_generic_non_ZIO_method_with_multiple_arg_lists_default_args_and_varargs()
    : Unit = {
    assertEquals(
      """def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*) = zio.ZIO.service[Example.Service].map(_.m2NonZIO[T](s2)(p)(i2: _*))""",
      method("m2NonZIO").getText
    )
    assertEquals(
      Right("ZIO[Has[Example.Service], Nothing, Double]"),
      method("m2NonZIO").returnType.map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_ZIO_method_returning_stream(): Unit = {
    assertEquals(
      """def stream(n: Int) = zio.ZIO.service[Example.Service].map(_.stream(n))""",
      method("stream").getText
    )
    assertEquals(
      Right("Int => ZIO[Has[Example.Service], Nothing, ZStream[Any, String, Int]]"),
      method("stream").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_value_for_non_ZIO_field_with_default_implementation(): Unit = {
    assertEquals(
      "val defaultImpureValue = zio.ZIO.service[Example.Service].map(_.defaultImpureValue)",
      field("defaultImpureValue").getText
    )
    assertEquals(
      Right("ZIO[Has[Example.Service], Nothing, Boolean]"),
      field("defaultImpureValue").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_non_ZIO_method_with_argument_with_default_implementation(): Unit = {
    assertEquals(
      "def defaultImpureMethod(s: String) = zio.ZIO.service[Example.Service].map(_.defaultImpureMethod(s))",
      method("defaultImpureMethod").getText
    )
    assertEquals(
      Right("String => ZIO[Has[Example.Service], Nothing, Int]"),
      method("defaultImpureMethod").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_value_for_managed_field(): Unit = {
    assertEquals(
      "val vManaged = zio.ZManaged.service[Example.Service].flatMap(_.vManaged)",
      field("vManaged").getText
    )
    assertEquals(
      Right("ZManaged[Has[Example.Service], String, Example.Foo]"),
      field("vManaged").`type`().map(_.codeText)
    )
  }

  def test_generates_accessor_function_for_method_returning_managed(): Unit = {
    assertEquals(
      "def mManaged(s: String) = zio.ZManaged.service[Example.Service].flatMap(_.mManaged(s))",
      method("mManaged").getText
    )
    assertEquals(
      Right("String => ZManaged[Has[Example.Service], Nothing, Example.Bar]"),
      method("mManaged").`type`().map(_.codeText)
    )
  }

}
