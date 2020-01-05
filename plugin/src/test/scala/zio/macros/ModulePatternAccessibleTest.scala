package zio.macros

import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.util.PsiTreeUtil
import intellij.testfixtures._
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.macros.SynteticInjectorsTestUtils._
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScAnnotation, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.PhysicalMethodSignature
import org.junit.Assert._

class ModulePatternAccessibleTest extends ScalaLightCodeInsightFixtureTestAdapter {
  private val caret          = "<caret>"
  private val annotationQual = "zio.macros.annotation.accessible"

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader("dev.zio" %% "zio-macros-core" % "0.5.0")

  def fromAnnotation(clazz: ScTypeDefinition): Option[String] =
    clazz.annotations(annotationQual).headOption.flatMap {
      case annotation: ScAnnotation =>
        annotation.annotationExpr.getAnnotationParameters.headOption.map {
          case lit: ScLiteral => lit.getValue().toString
        }
      case _ => None
    }

  def accessor(text: String): ScFunctionDefinition = {
    val cleaned  = StringUtil.convertLineSeparators(text)
    val caretPos = cleaned.indexOf(caret)
    getFixture.configureByText("dummy.scala", cleaned.replace(caret, ""))

    val clazz = PsiTreeUtil.findElementOfClassAtOffset(
      getFile,
      caretPos,
      classOf[ScTypeDefinition],
      false
    )

    val accessorName = fromAnnotation(clazz)
      .getOrElse(fail(s"Unable to extract the companion name from the '${annotationQual}' annotation argument"))

    val accessorDef = ScalaPsiUtil
      .getCompanionModule(clazz)
      .getOrElse(clazz.asInstanceOf[ScObject])
      .allMethods
      .collectFirst {
        case PhysicalMethodSignature(fun: ScFunctionDefinition, _) if fun.name == accessorName => fun
      }

    accessorDef
      .getOrElse(
        fail(s"Accessor definition '$accessorName' was not found inside the companion object of ${clazz.name}")
          .asInstanceOf[ScFunctionDefinition]
      )
  }

  def test_generates_accessor_function_in_companion(): Unit = {
    val code =
      s"""
import zio.macros.annotation.accessible

@accessible(">")
trait E${caret}xample {
  val example: Example.Service[Any]
}

object Example {
  trait Service[R] {}
}
"""

    accessor(code) mustBeExactly `def`(">", "Example.Service[Example]")
  }
}
