package zio.macros

import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.util.PsiTreeUtil
import intellij.testfixtures._
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{ IvyManagedLoader, LibraryLoader }
import org.jetbrains.plugins.scala.lang.macros.SynteticInjectorsTestUtils._
import org.jetbrains.plugins.scala.lang.psi.api.base.{ ScAnnotation, ScLiteral }
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.PhysicalMethodSignature
import org.junit.Assert._

class ModulePatternAccessibleTest extends ScalaLightCodeInsightFixtureTestAdapter {
  private val caret          = "<caret>"
  private val annotationQual = "zio.macros.annotation.accessible"

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader("dev.zio" %% "zio-macros-core" % "0.5.0")

  def fromAnnotation(obj: ScObject): Option[String] =
    obj.annotations(annotationQual).headOption.flatMap {
      case annotation: ScAnnotation =>
        annotation.annotationExpr.getAnnotationParameters.headOption.map {
          case lit: ScLiteral => lit.getValue().toString
        }
      case _ => None
    }

  def accessor(text: String): ScObject = {
    val cleaned  = StringUtil.convertLineSeparators(text)
    val caretPos = cleaned.indexOf(caret)
    getFixture.configureByText("dummy.scala", cleaned.replace(caret, ""))

    val obj = PsiTreeUtil.findElementOfClassAtOffset(
      getFile,
      caretPos,
      classOf[ScObject],
      false
    )

    val accessorName = fromAnnotation(obj)
      .getOrElse(fail(s"Unable to extract the companion name from the '$annotationQual' annotation argument"))

    val accessorDef = obj.allInnerTypeDefinitions
      .collectFirst {
        case o: ScObject if o.name == accessorName => o
      }

    accessorDef
      .getOrElse(
        fail(s"Accessor definition '$accessorName' was not found inside object ${obj.name}")
          .asInstanceOf[ScObject]
      )
  }

  def method(obj: ScObject, name: String): ScFunctionDefinition =
    obj.allMethods
      .collectFirst {
        case PhysicalMethodSignature(fun: ScFunctionDefinition, _) if fun.name == name => fun
      }
      .getOrElse(
        fail(s"Method declaration $name was not found inside object ${obj.name}")
          .asInstanceOf[ScFunctionDefinition]
      )

  def test_generates_accessor_function_in_companion(): Unit = {
    val code =
      s"""
import zio._
import zio.blocking.Blocking
import zio.macros.annotation.accessible

@accessible(">")
object E${caret}xample {
  type Environment = Blocking

  type EIO[+T] = ZIO[Environment, Nothing, T]

  trait Service {
    val v: EIO[Boolean]
    def m0: EIO[Unit]
    def m1(s: String): EIO[Int]
    def m3[T](s2: String = "")(p: (T, Int))(i2: Int*): EIO[Double]
  }
}
"""

    val scObject = accessor(code)
    scObject mustBeExactly `object`(">").copy(functions = Seq(
      `def`("m0", "Any"),
      `def`("m1", "_root_.scala.Predef.String => Any"),
      `def`("m3", "[T] String => ((T, Int)) => Int => Any")
    )
    )

    assertEquals(
      "def m0 = zio.ZIO.access[zio.Has[Example.Service]](_.get).flatMap(_.m0)",
      method(scObject, "m0").getText
    )
    assertEquals(
      "def m1(s: String) = zio.ZIO.access[zio.Has[Example.Service]](_.get).flatMap(_.m1(s))",
      method(scObject, "m1").getText
    )
    assertEquals(
      """def m3[T](s2: String = "")(p: (T, Int))(i2: Int*) = zio.ZIO.access[zio.Has[Example.Service]](_.get).flatMap(_.m3[T](s2)(p)(i2: _*))""",
      method(scObject, "m3").getText
    )
  }
}
