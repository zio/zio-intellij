package zio.macros

import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.util.PsiTreeUtil
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunctionDefinition, ScPatternDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, TypePresentationContext}
import org.jetbrains.plugins.scala.DependencyManagerBase._
import org.junit.Assert.fail
import zio.inspections.ZInspectionTestBase

abstract class MacrosTest extends ScalaLightCodeInsightFixtureTestAdapter {
  protected val zioOrg     = "dev.zio"
  protected def zioVersion = ZInspectionTestBase.versionPattern

  protected var extendedObject: ScObject                                  = _
  implicit protected var typePresentationContext: TypePresentationContext = _

  protected def code: String

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader(zioOrg %% "zio" % zioVersion)

  override def setUp(): Unit = {
    super.setUp()

    val cleaned  = StringUtil.convertLineSeparators(code)
    val caretPos = cleaned.indexOf(CARET)
    configureFromFileText(cleaned.replace(CARET, ""))

    extendedObject = PsiTreeUtil.findElementOfClassAtOffset(
      getFile,
      caretPos,
      classOf[ScObject],
      false
    )
    typePresentationContext = TypePresentationContext(extendedObject)
  }

  protected def method(name: String, obj: ScObject = extendedObject): ScFunctionDefinition =
    obj.allMethods.collectFirst {
      case PhysicalMethodSignature(fun: ScFunctionDefinition, _) if fun.name == name => fun
    }.getOrElse {
      fail(s"Method declaration $name was not found inside object ${obj.name}")
        .asInstanceOf[ScFunctionDefinition]
    }

  protected def field(name: String, obj: ScObject = extendedObject): ScPatternDefinition =
    obj.membersWithSynthetic.collectFirst {
      case pd: ScPatternDefinition if pd.isSimple && pd.bindings.head.name == name => pd
    }
      .getOrElse(
        fail(s"Field $name was not found inside object ${obj.name}")
          .asInstanceOf[ScPatternDefinition]
      )

  protected def innerObject(name: String, obj: ScObject = extendedObject): ScObject =
    obj.membersWithSynthetic.collectFirst {
      case o: ScObject if o.name == name => o
    }
      .getOrElse(
        fail(s"Inner object $name was not found inside object ${obj.name}")
          .asInstanceOf[ScObject]
      )

}
