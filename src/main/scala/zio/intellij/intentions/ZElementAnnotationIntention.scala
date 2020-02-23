package zio.intellij.intentions

import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl
import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction
import com.intellij.lang.annotation.{Annotation, AnnotationSession}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiDocumentManager, PsiElement}
import org.jetbrains.plugins.scala.annotator.element.ElementAnnotator
import org.jetbrains.plugins.scala.lang.psi.api.ScalaPsiElement
import scala.reflect._
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

abstract class ZElementAnnotationIntention[T <: ScalaPsiElement: ClassTag] extends PsiElementBaseIntentionAction with ZIcon {
  override def getText: String = getFamilyName

  protected def problemAnnotations(
    element: PsiElement,
    project: Project,
    editor: Editor
  ): Option[(T, List[Annotation])] = {
    def annotationsFor(elem: T): List[Annotation] = {
      val file   = PsiDocumentManager.getInstance(project).getPsiFile(editor.getDocument)
      val holder = new AnnotationHolderImpl(new AnnotationSession(file))
      ElementAnnotator.annotate(elem, typeAware = true)(holder)
      holder.asScala.toList
    }

    Option(PsiTreeUtil.getParentOfType(element, classTag[T].runtimeClass.asInstanceOf[Class[T]]))
      .map(elem => (elem, annotationsFor(elem)))
  }

}
