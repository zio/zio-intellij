package zio.intellij.testsupport

import com.intellij.codeInsight.TestFrameworks
import com.intellij.navigation.ItemPresentation
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiClass, PsiFile, PsiPackage}
import com.intellij.testIntegration.createTest.{CreateTestAction, CreateTestDialog}
import com.intellij.util.IncorrectOperationException
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.project.ProjectExt
import org.jetbrains.plugins.scala.testingSupport.ScalaTestCreator
import zio.intellij.ZioIcon
import zio.intellij.utils.ModuleSyntax

import javax.swing.Icon

// TODO this class is a hack on top of ScalaTestCreator *just* to have the default suffix for tests be `Specs`.
// Otherwise, this class is a copy of ScalaTestCreator, with some added presentation.
// Unfortunately, there doesn't seem to be a way to *disable* the built-in ScalaTestCreator item, so the prompt
// to create a new test (Cmd/Ctrl+Shift+T) will always have two items. At least ours is prettier :)
final class ZTestCreator extends ScalaTestCreator with ItemPresentation {

  override def createTest(project: Project, editor: Editor, file: PsiFile): Unit = {
    import ZTestCreator._
    try {
      val action = new CreateTestAction {
        override protected def createTestDialog(
          project: Project,
          srcModule: Module,
          srcClass: PsiClass,
          srcPackage: PsiPackage
        ): CreateTestDialog =
          new CreateTestDialog(project, getText, srcClass, srcPackage, srcModule) {
            override def suggestTestClassName(targetClass: PsiClass): String = targetClass match {
              case obj: ScTypeDefinition =>
                obj.name.stripSuffix("$") + "Spec" // Y U NOT LET ME CONFIGURE THIS JETBRAIN!
              case _ => super.suggestTestClassName(targetClass)
            }
          }
      }
      val element = findElement(file, editor.getCaretModel.getOffset)
      if (CreateTestAction.isAvailableForElement(element)) action.invoke(project, editor, element)
    } catch {
      case e: IncorrectOperationException => LOG.warn(e)
    }
  }

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    project.modules.exists(_.hasZio) && super.isAvailable(project, editor, file)

  override def getPresentableText: String = "Create New ZIO Spec..."

  override def getIcon(unused: Boolean): Icon = ZioIcon
}

object ZTestCreator {
  private val LOG = Logger.getInstance("zio.intellij.testsupport.ZTestCreator")
  private def findElement(file: PsiFile, offset: Int) = {
    var element = file.findElementAt(offset)
    if (element == null && offset == file.getTextLength) element = file.findElementAt(offset - 1)
    element
  }
}
