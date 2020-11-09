package zio.intellij.gutter

import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.openapi.editor.markup.GutterIconRenderer.Alignment
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScReferenceExpression
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._

final class ForkedCodeLineMarkerProvider extends LineMarkerProvider {
  import zio.intellij.gutter.ForkedCodeLineMarkerProvider.createLineMarkerInfo

  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[_ <: PsiElement] =
    if (
      !element.isValid ||
      element.getNode.getElementType != ScalaTokenTypes.tIDENTIFIER ||
      // fixme: It is recommended to use leaf elements, such as identifiers.
      //        But in 'ZIO.forkAll' ZIO considered an identifier as well.
      //        Thus we need to keep only one marker somehow (for now: by checking 'fork' prefix).
      !element.getText.startsWith("fork")
    )
      null
    else
      element.getParent match {
        case `.fork`(_) | `.forkDaemon`(_) | `.forkManaged`(_) => createLineMarkerInfo(element)
        case ref: ScReferenceExpression =>
          ref.getParent match {
            case `.forkAs`(_) | `.forkOn`(_) | `.forkWithErrorHandler`(_) | `ZIO.forkAll`(_, _) |
                `ZIO.forkAll_`(_, _) =>
              createLineMarkerInfo(element)
            case _ => null
          }
        case _ => null
      }
}

object ForkedCodeLineMarkerProvider {

  def createLineMarkerInfo(element: PsiElement): LineMarkerInfo[PsiElement] =
    new LineMarkerInfo(
      element,
      element.getTextRange,
      fiberIcon,
      (_: PsiElement) => s"Effect is explicitly forked via '${element.getText}'",
      null, // the handler executed when the gutter icon is clicked
      Alignment.LEFT,
      () => s"Effect is explicitly forked via '${element.getText}'"
    )

}
