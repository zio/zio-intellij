package zio.intellij.intentions.migrations

import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl
import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.lang.annotation.{Annotation, AnnotationSession}
import com.intellij.openapi.editor.ex.util.EditorUIUtil
import com.intellij.openapi.editor.{Caret, Editor, EditorModificationUtil}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Iconable
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement, PsiMember}
import com.intellij.refactoring.HelpID
import com.intellij.refactoring.move.moveMembers.{MoveMemberHandler, MoveMembersOptions}
import javax.swing.Icon
import org.jetbrains.plugins.scala.annotator.element.ScConstructorInvocationAnnotator
import org.jetbrains.plugins.scala.extensions.executeWriteActionCommand
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil
import org.jetbrains.plugins.scala.lang.psi.api.base.ScConstructorInvocation
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory._
import org.jetbrains.plugins.scala.lang.refactoring.introduceField._
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil
import org.jetbrains.plugins.scala.lang.refactoring.util.ScalaRefactoringUtil._

import scala.collection.JavaConverters._


// this is a mess...
final class MigrateTestSuiteToSpecField extends PsiElementBaseIntentionAction with Iconable {
  val ProblemText = "DefaultRunnableSpec is a trait and thus has no constructor"

  override def getFamilyName: String = "Migrate suite to the 'val spec = ...' syntax"

  override def getText: String = getFamilyName

  override def invoke(project: Project, editor: Editor, element: PsiElement): Unit =
    problemAnnotation(element, project, editor).foreach {
      case (ctor, annotation) =>
        val file = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument)
        ctor.arguments.flatMap(_.exprs).headOption.map(_.getTextRange).foreach { range =>
          val startOffset = range.getStartOffset
          val endOffset   = range.getEndOffset
          editor.getSelectionModel.setSelection(startOffset, endOffset)
          new IntroduceSpecFieldRefactoring(annotation)
            .invoke(file, startOffset, endOffset)(project, editor)
        }

    }

  override def isAvailable(project: Project, editor: Editor, element: PsiElement): Boolean =
    problemAnnotation(element, project, editor).isDefined

  private def problemAnnotation(
    element: PsiElement,
    project: Project,
    editor: Editor
  ): Option[(ScConstructorInvocation, Annotation)] = {
    def annotationsFor(ctor: ScConstructorInvocation): List[Annotation] = {
      val file   = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument)
      val holder = new AnnotationHolderImpl(new AnnotationSession(file))
      ScConstructorInvocationAnnotator.annotate(ctor, typeAware = true)(holder)
      holder.asScala.toList
    }

    for {
      ctor <- Option(PsiTreeUtil.getParentOfType(element, classOf[ScConstructorInvocation]))
      ann  <- annotationsFor(ctor).find(_.getMessage == ProblemText)
    } yield (ctor, ann)
  }

  override def getIcon(flags: Int): Icon = zio.intellij.ZioIcon
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
