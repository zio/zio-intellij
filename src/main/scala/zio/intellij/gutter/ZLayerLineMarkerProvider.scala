package zio.intellij.gutter

import com.intellij.codeInsight.daemon.{LineMarkerInfo, LineMarkerProvider}
import com.intellij.openapi.editor.markup.GutterIconRenderer.Alignment
import com.intellij.openapi.fileEditor.FileEditor
import com.intellij.openapi.fileEditor.impl.HTMLEditorProvider
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement
import com.intellij.ui.jcef.JBCefApp
import com.intellij.util.Urls
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenericCall, ScMethodCall}
import zio.intellij.icons.LayersIcon
import zio.intellij.inspections._
import zio.intellij.inspections.macros.MacroGraphUtils
import zio.intellij.inspections.zioMethods._

import java.awt.event.MouseEvent

final class ZLayerLineMarkerProvider extends LineMarkerProvider {
  import zio.intellij.gutter.ZLayerLineMarkerProvider.createLineMarkerInfo

  override def getLineMarkerInfo(element: PsiElement): LineMarkerInfo[_ <: PsiElement] = element match {
    case `.inject`(base, layers @ _*) =>
      createLineMarkerInfo(base, layers)
    case ScMethodCall(partiallyApplied @ ScGenericCall(`ZLayer.makeLike`(_, _), _), layers) =>
      createLineMarkerInfo(partiallyApplied, layers)
    case _ => null
  }

}
object ZLayerLineMarkerProvider {
  def createLineMarkerInfo(element: ScExpression, layers: Seq[ScExpression]): LineMarkerInfo[PsiElement] =
    new LineMarkerInfo(
      element,
      element.getTextRange,
      LayersIcon,
      (_: PsiElement) => "Show ZLayer build graph (Mermaid)",
      (_: MouseEvent, _: PsiElement) =>
        MacroGraphUtils.renderMermaid(element) match {
          case Some(mermaid) =>
            browse(element.getProject, mermaid): Unit
          case None =>
        },
      Alignment.LEFT,
      () => "Show ZLayer build graph"
    )

  def browse(project: Project, url: String): FileEditor = {
    if (!JBCefApp.isSupported) throw new IllegalStateException("JCEF is not supported on this system")
    val request: HTMLEditorProvider.Request = HTMLEditorProvider.Request.url(Urls.newFromEncoded(url).toExternalForm)
    request.withQueryHandler(null)
    HTMLEditorProvider.openEditor(project, "ZLayer Mermaid Diagram", request)
  }
}
