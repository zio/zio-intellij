package zio.intellij.intentions.migrations

import com.intellij.lang.annotation.Annotation
import com.intellij.openapi.editor.ex.util.EditorUIUtil
import com.intellij.openapi.editor.{Caret, Editor, EditorModificationUtil}
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiMember}
import com.intellij.refactoring.HelpID
import com.intellij.refactoring.move.moveMembers.{MoveMemberHandler, MoveMembersOptions}
import org.jetbrains.plugins.scala.extensions.executeWriteActionCommand
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructorInvocation
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory._
import org.jetbrains.plugins.scala.lang.refactoring.introduceField._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._
import zio.intellij.intentions.ZElementAnnotationIntention

final class MigrateTestSuiteToSpecField extends ZElementAnnotationIntention[ScConstructorInvocation] {
  val ProblemText = "DefaultRunnableSpec is a trait and thus has no constructor"

  override def getFamilyName: String = "Migrate suite to the 'val spec = ...' syntax"

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    problemAnnotations(element, project, editor).foreach {
      case (ctor, List(annotation)) =>
        val file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument)
        ctor.arguments.flatMap(_.exprs).headOption.map(_.getTextRange).foreach { range =>
          val startOffset = range.getStartOffset
          val endOffset   = range.getEndOffset
          editor.getSelectionModel.setSelection(startOffset, endOffset)
          new IntroduceSpecFieldRefactoring(annotation)
            .invoke(file, startOffset, endOffset)(project, editor)
        }
      case _ =>
    }

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    problemAnnotations(element, project, editor) match {
      case Some((_, List(_))) => true
      case _ => false
    }
}

final private[this] class IntroduceSpecFieldRefactoring(annotation: Annotation)
    extends ScalaIntroduceFieldFromExpressionHandler {

  override def convertExpressionToField(ifc: IntroduceFieldContext[ScExpression]): Unit = {
    implicit val project: Project = ifc.project
    implicit val editor: Editor   = ifc.editor

    val settings = new IntroduceFieldSettings(ifc)
    if (settings.canBeInitInDeclaration || settings.canBeInitLocally) {
      executeWriteActionCommand(REFACTORING_NAME) {
        doRefactoring(ifc, settings)
      }(ifc.project)
      ifc.editor.getSelectionModel.removeSelection()
    } else {
      showErrorHint("Cannot create field from this expression")
    }
  }

  def doRefactoring(ifc: IntroduceFieldContext[ScExpression], settings: IntroduceFieldSettings[ScExpression]): Unit = {
    implicit val project: Project = ifc.project
    implicit val editor: Editor   = ifc.editor

    val expression = expressionToIntroduce(ifc.element)
    val aClass     = ifc.aClass
    val specField  = createDeclaration("spec", "", isVariable = false, expression)

    editor.getSelectionModel.setSelection(annotation.getStartOffset, annotation.getEndOffset)

    EditorUIUtil.hideCursorInEditor(editor)
    editor.getCaretModel.runForEachCaret((_: Caret) => EditorModificationUtil.deleteSelectedText(editor))
    PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument)

    val handler = MoveMemberHandler.EP_NAME.forLanguage(specField.getLanguage)
    handler.doMove(moveMemberOptions(aClass.name), specField.asInstanceOf[PsiMember], null, aClass)

    ScalaPsiUtil.adjustTypes(specField)
  }

  private def moveMemberOptions(targetClass: String) = new MoveMembersOptions {
    override def getSelectedMembers: Array[PsiMember] = Array.empty
    override def getTargetClassName: String           = targetClass
    override def getMemberVisibility: String          = ""
    override def makeEnumConstant(): Boolean          = false
  }

  private def showErrorHint(message: String)(implicit project: Project, editor: Editor): Unit =
    ScalaRefactoringUtil.showErrorHint(message, REFACTORING_NAME, HelpID.INTRODUCE_FIELD)
}
