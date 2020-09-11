package zio.macros

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.junit.Assert.{assertEquals, assertTrue}
import zio.intellij.utils.createType

class MockableInjectorTest extends MacrosTest {

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader(zioOrg %% "zio-streams" % zioVersion, zioOrg %% "zio-test" % zioVersion)

  override protected val code =
    s"""import zio._
       |import zio.stream._
       |import zio.test.mock._
       |
       |object ServiceObject {
       |  trait Foo {
       |    val value: String
       |  }
       |  final case class Bar(value: String) extends Foo
       |  final case class Wrapped[T](value: T)
       |
       |  trait Service {
       |    def m1: Int // Method[Unit, Throwable, Int]
       |    def m2(s: String): Boolean // Method[String, Throwable, Boolean]
       |    def m3(i: Int)(b: Boolean, s: String*): Option[(Int, Double)] // Method[Tuple3[Int, Boolean, Seq[String]], Throwable, Option[(Int, Double)]]
       |    val v1: Int // Method[Unit, Throwable, Int]
       |
       |    def polyM1[A: Tag](a: A): Boolean // Poly.Method.Input[Throwable, Boolean]
       |    def polyM2[A: Tag](o: Option[String]): A // Poly.Method.Output[Option[String], Throwable]
       |    def polyM3[A: Tag, B: Tag](s: Seq[A]): B // Poly.Method.InputOutput[Throwable]
       |
       |    def e1(s: String): UIO[Int]  // Effect[String, Nothing, Int]
       |    def e2: Task[Long] // Effect[Unit, Throwable, Long]
       |    def e3: IO[String, Long] // Effect[Unit, String, Long]
       |    def e4: URIO[String, Long] // Effect[Unit, Nothing, Long]
       |    val v2: UIO[Either[Int, String]] // Effect[Unit, Nothing, Either[Int, String]]
       |
       |    def poly1[A: Tag](a: A): UIO[Double] // Poly.Effect.Input[Nothing, Double]
       |    def poly2[A: Tag]: IO[A, String] // Poly.Effect.Error[Unit, String]
       |    def poly3[A: Tag]: UIO[A] // Poly.Effect.Output[Unit, Nothing]
       |    def poly4[A: Tag, B: Tag](a: A): IO[B, Boolean] // Poly.Effect.InputError[Boolean]
       |    def poly5[A: Tag, B: Tag](a: A): IO[Int, B] // Poly.Effect.InputOutput[Int]
       |    def poly6[A: Tag, B: Tag]: IO[A, B] // Poly.Effect.ErrorOutput[Unit]
       |    def poly7[A: Tag, B: Tag, C: Tag](a: A): IO[B, C] // Poly.Effect.InputErrorOutput
       |
       |    def poly8[A: Tag]: UIO[(A, String)] // Poly.Effect.Output[Unit, Nothing]
       |    def poly9[A <: Foo : Tag]: UIO[A] // Poly.Effect.Output[Unit, Nothing]
       |    def poly10[A: Tag](a: Wrapped[A]): UIO[A] // Poly.Effect.InputOutput[Nothing]
       |
       |    def overloaded(l: Long, os: Option[String], ob: Option[Boolean]): Task[Option[Unit]] // _1 // Effect[Tuple3[Long, Option[String], Option[Boolean]], Throwable, Option[Unit]]
       |    def overloaded(l: Long, s: String): Task[Option[Unit]] // _2 // Effect[Tuple2[Long, String], Throwable, Option[Unit]]
       |    val overloaded: Task[Option[Unit]] // _0 // Effect[Unit, Throwable, Option[Unit]]
       |
       |    val staticStream: Stream[String, Int]
       |    def zeroParamsStream: Stream[String, Int]
       |    def zeroParamsWithParensStream(): Stream[String, Int]
       |    def singleParamStream(a: Int): Stream[String, Int]
       |    def manyParamsStream(a: Int, b: String, c: Long): Stream[String, Int]
       |    def manyParamListsStream(a: Int)(b: String)(c: Long): Stream[String, Int]
       |
       |    def polyInputStream[I: Tag](v: I): Stream[String, Int]
       |    def polyErrorStream[E: Tag](v: Long): Stream[E, Int]
       |    def polyOutputStream[A: Tag](v: Long): Stream[String, A]
       |    def polyInputErrorStream[I: Tag, E: Tag](v: I): Stream[E, Int]
       |    def polyInputOutputStream[I: Tag, A: Tag](v: I): Stream[String, A]
       |    def polyErrorOutputStream[E: Tag, A: Tag](v: Long): Stream[E, A]
       |    def polyInputErrorOutputStream[I: Tag, E: Tag, A: Tag](v: I): Stream[E, A]
       |    def polyMixedStream[A: Tag]: Stream[String, (A, Int)]
       |    def polyBoundedStream[A <: AnyVal: Tag]: Stream[String, A]
       |  }
       |}
       |
       |@mockable[ServiceObject.Service]
       |object Se${CARET}rviceMock""".stripMargin

  def test_injected_compose_value(): Unit = {
    val compose = field("compose")
    val expectedTypeOpt =
      createType("zio.URLayer[zio.Has[zio.test.mock.Proxy], zio.Has[ServiceObject.Service]]", context = extendedObject)
    assertTrue(expectedTypeOpt.isDefined)
    val expectedType = expectedTypeOpt.get
    assertTrue(compose.`type`().isRight)
    val actualType = compose.`type`().get
    assertTrue(actualType.conforms(expectedType))
  }

  def test_impure_method_without_args(): Unit =
    assertEquals(
      "extends Method[Unit, Throwable, Int]",
      innerObject("M1").extendsBlock.getText
    )

  def test_impure_method_with_one_argument(): Unit =
    assertEquals(
      "extends Method[String, Throwable, Boolean]",
      innerObject("M2").extendsBlock.getText
    )

  def test_impure_method_with_multiple_argument_lists_and_varargs(): Unit =
    assertEquals(
      "extends Method[(Int, Boolean, Seq[String]), Throwable, Option[(Int, Double)]]",
      innerObject("M3").extendsBlock.getText
    )

  def test_impure_value(): Unit =
    assertEquals(
      "extends Method[Unit, Throwable, Int]",
      innerObject("V1").extendsBlock.getText
    )

  def test_impure_method_with_polymorphic_input(): Unit =
    assertEquals(
      "extends Poly.Method.Input[Throwable, Boolean]",
      innerObject("PolyM1").extendsBlock.getText
    )

  def test_impure_method_with_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Method.Output[Option[String], Throwable]",
      innerObject("PolyM2").extendsBlock.getText
    )

  def test_impure_method_with_polymorphic_input_and_output(): Unit =
    assertEquals(
      "extends Poly.Method.InputOutput[Throwable]",
      innerObject("PolyM3").extendsBlock.getText
    )

  def test_uio_method_with_one_argument(): Unit =
    assertEquals(
      "extends Effect[String, Nothing, Int]",
      innerObject("E1").extendsBlock.getText
    )

  def test_task_method(): Unit =
    assertEquals(
      "extends Effect[Unit, Throwable, Long]",
      innerObject("E2").extendsBlock.getText
    )

  def test_io_method(): Unit =
    assertEquals(
      "extends Effect[Unit, String, Long]",
      innerObject("E3").extendsBlock.getText
    )

  def test_urio_method(): Unit =
    assertEquals(
      "extends Effect[Unit, Nothing, Long]",
      innerObject("E4").extendsBlock.getText
    )

  def test_uio_value(): Unit =
    assertEquals(
      "extends Effect[Unit, Nothing, Either[Int, String]]",
      innerObject("V2").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_input(): Unit =
    assertEquals(
      "extends Poly.Effect.Input[Nothing, Double]",
      innerObject("Poly1").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_error(): Unit =
    assertEquals(
      "extends Poly.Effect.Error[Unit, String]",
      innerObject("Poly2").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Effect.Output[Unit, Nothing]",
      innerObject("Poly3").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_input_and_error(): Unit =
    assertEquals(
      "extends Poly.Effect.InputError[Boolean]",
      innerObject("Poly4").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_input_and_output(): Unit =
    assertEquals(
      "extends Poly.Effect.InputOutput[Int]",
      innerObject("Poly5").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_error_and_output(): Unit =
    assertEquals(
      "extends Poly.Effect.ErrorOutput[Unit]",
      innerObject("Poly6").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_input_error_and_output(): Unit =
    assertEquals(
      "extends Poly.Effect.InputErrorOutput",
      innerObject("Poly7").extendsBlock.getText
    )

  def test_pure_method_with_mixed_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Effect.Output[Unit, Nothing]",
      innerObject("Poly8").extendsBlock.getText
    )

  def test_pure_method_with_bounded_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Effect.Output[Unit, Nothing]",
      innerObject("Poly9").extendsBlock.getText
    )

  def test_pure_method_with_polymorphic_output_and_wrapped_polymorphic_input(): Unit =
    assertEquals(
      "extends Poly.Effect.InputOutput[Nothing]",
      innerObject("Poly10").extendsBlock.getText
    )

  def test_overloaded_methods(): Unit = {
    val overloaded = innerObject("Overloaded")
    assertEquals(
      "extends Effect[Unit, Throwable, Option[Unit]]",
      innerObject("_0", overloaded).extendsBlock.getText
    )
    assertEquals(
      "extends Effect[(Long, Option[String], Option[Boolean]), Throwable, Option[Unit]]",
      innerObject("_1", overloaded).extendsBlock.getText
    )
    assertEquals(
      "extends Effect[(Long, String), Throwable, Option[Unit]]",
      innerObject("_2", overloaded).extendsBlock.getText
    )
  }

  def test_stream_value(): Unit =
    assertEquals(
      "extends Stream[Unit, String, Int]",
      innerObject("StaticStream").extendsBlock.getText
    )

  def test_stream_method_without_arguments(): Unit =
    assertEquals(
      "extends Stream[Unit, String, Int]",
      innerObject("ZeroParamsStream").extendsBlock.getText
    )

  def test_stream_method_without_arguments_and_with_parens(): Unit =
    assertEquals(
      "extends Stream[Unit, String, Int]",
      innerObject("ZeroParamsWithParensStream").extendsBlock.getText
    )

  def test_stream_method_with_one_argument(): Unit =
    assertEquals(
      "extends Stream[Int, String, Int]",
      innerObject("SingleParamStream").extendsBlock.getText
    )

  def test_stream_method_with_multiple_arguments(): Unit =
    assertEquals(
      "extends Stream[(Int, String, Long), String, Int]",
      innerObject("ManyParamsStream").extendsBlock.getText
    )

  def test_stream_method_with_multiple_argument_lists(): Unit =
    assertEquals(
      "extends Stream[(Int, String, Long), String, Int]",
      innerObject("ManyParamListsStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_input(): Unit =
    assertEquals(
      "extends Poly.Stream.Input[String, Int]",
      innerObject("PolyInputStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_error(): Unit =
    assertEquals(
      "extends Poly.Stream.Error[Long, Int]",
      innerObject("PolyErrorStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Stream.Output[Long, String]",
      innerObject("PolyOutputStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_input_and_error(): Unit =
    assertEquals(
      "extends Poly.Stream.InputError[Int]",
      innerObject("PolyInputErrorStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_input_and_output(): Unit =
    assertEquals(
      "extends Poly.Stream.InputOutput[String]",
      innerObject("PolyInputOutputStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_error_and_output(): Unit =
    assertEquals(
      "extends Poly.Stream.ErrorOutput[Long]",
      innerObject("PolyErrorOutputStream").extendsBlock.getText
    )

  def test_stream_method_with_polymorphic_input_error_and_output(): Unit =
    assertEquals(
      "extends Poly.Stream.InputErrorOutput",
      innerObject("PolyInputErrorOutputStream").extendsBlock.getText
    )

  def test_stream_method_with_mixed_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Stream.Output[Unit, String]",
      innerObject("PolyMixedStream").extendsBlock.getText
    )

  def test_stream_method_with_bounded_polymorphic_output(): Unit =
    assertEquals(
      "extends Poly.Stream.Output[Unit, String]",
      innerObject("PolyBoundedStream").extendsBlock.getText
    )

}
