package zio.macros

import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.util.PsiTreeUtil
import intellij.testfixtures._
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.PhysicalMethodSignature
import org.junit.Assert._

class ModulePatternAccessibleTest extends ScalaLightCodeInsightFixtureTestAdapter {
  private val caret = "<caret>"

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader("dev.zio" %% "zio"        % "1.0.0-RC20") :+
      IvyManagedLoader("dev.zio" %% "zio-macros" % "1.0.0-RC20")

  private var extendedObject: ScObject = _

  override def setUp(): Unit = {
    super.setUp()

    val code =
      s"""
import zio._
import zio.blocking.Blocking
import zio.macros.accessible

@accessible
object E${caret}xample {
  type Environment = Blocking

  type EIO[+T] = ZIO[Environment, Nothing, T]

  trait Service {
    val v: EIO[Boolean]
    def m0: EIO[Unit]
    def m1(s: String): EIO[Int]
    def m2[T](s2: String = "")(p: (T, Int))(i2: Int*): EIO[Double]

    val vNonZIO: Boolean
    def m0NonZIO: Unit
    def m1NonZIO(s: String): Int
    def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*): Double
  }
}
"""
    val cleaned  = StringUtil.convertLineSeparators(code)
    val caretPos = cleaned.indexOf(caret)
    configureFromFileText(cleaned.replace(caret, ""))

    extendedObject = PsiTreeUtil.findElementOfClassAtOffset(
      getFile,
      caretPos,
      classOf[ScObject],
      false
    )
  }

  private def method(name: String): ScFunctionDefinition =
    extendedObject.allMethods
      .collectFirst {
        case PhysicalMethodSignature(fun: ScFunctionDefinition, _) if fun.name == name => fun
      }
      .getOrElse(
        fail(s"Method declaration $name was not found inside object ${extendedObject.name}")
          .asInstanceOf[ScFunctionDefinition]
      )

  private def field(name: String): ScPatternDefinition =
    extendedObject.membersWithSynthetic
      .collectFirst {
        case pd: ScPatternDefinition if pd.isSimple && pd.bindings.head.name == name => pd
      }
      .getOrElse(
        fail(s"Field $name was not found inside object ${extendedObject.name}")
          .asInstanceOf[ScPatternDefinition]
      )

  def test_generates_accessor_value_for_ZIO_field(): Unit = assertEquals(
    "val v = zio.ZIO.service[Example.Service].flatMap(_.v)",
    field("v").getText
  )

  def test_generates_accessor_function_for_ZIO_method_without_arguments(): Unit = assertEquals(
    "def m0 = zio.ZIO.service[Example.Service].flatMap(_.m0)",
    method("m0").getText
  )

  def test_generates_accessor_function_for_ZIO_method_with_argument(): Unit = assertEquals(
    "def m1(s: String) = zio.ZIO.service[Example.Service].flatMap(_.m1(s))",
    method("m1").getText
  )

  def test_generates_accessor_function_for_generic_ZIO_method_with_multiple_arg_lists_default_args_and_varargs(): Unit =
    assertEquals(
      """def m2[T](s2: String = "")(p: (T, Int))(i2: Int*) = zio.ZIO.service[Example.Service].flatMap(_.m2[T](s2)(p)(i2: _*))""",
      method("m2").getText
    )

  def test_generates_accessor_value_for_non_ZIO_field(): Unit = assertEquals(
    "val vNonZIO = zio.ZIO.service[Example.Service].map(_.vNonZIO)",
    field("vNonZIO").getText
  )

  def test_generates_accessor_function_for_non_ZIO_method_without_arguments(): Unit = assertEquals(
    "def m0NonZIO = zio.ZIO.service[Example.Service].map(_.m0NonZIO)",
    method("m0NonZIO").getText
  )

  def test_generates_accessor_function_for_non_ZIO_method_with_argument(): Unit = assertEquals(
    "def m1NonZIO(s: String) = zio.ZIO.service[Example.Service].map(_.m1NonZIO(s))",
    method("m1NonZIO").getText
  )

  def test_generates_accessor_function_for_generic_non_ZIO_method_with_multiple_arg_lists_default_args_and_varargs()
    : Unit = assertEquals(
    """def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*) = zio.ZIO.service[Example.Service].map(_.m2NonZIO[T](s2)(p)(i2: _*))""",
    method("m2NonZIO").getText
  )

}
