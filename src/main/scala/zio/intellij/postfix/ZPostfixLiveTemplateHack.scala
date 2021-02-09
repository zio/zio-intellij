package zio.intellij.postfix

import com.intellij.codeInsight.lookup.LookupElementPresentation
import com.intellij.codeInsight.template.CustomTemplateCallback
import com.intellij.codeInsight.template.impl.CustomLiveTemplateLookupElement
import com.intellij.codeInsight.template.postfix.completion.PostfixTemplateLookupElement
import com.intellij.codeInsight.template.postfix.templates.{PostfixLiveTemplate, PostfixTemplatesUtils}
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.util.{Disposer, Iconable}
import com.intellij.psi.PsiFile
import org.jetbrains.plugins.scala.project.ProjectPsiFileExt
import zio.intellij.ZioIcon
import zio.intellij.intentions.ZIcon
import zio.intellij.utils.ModuleSyntax

import java.util
import java.util.Collections
import scala.jdk.CollectionConverters._

/*
  The only reason this exists is to allow custom rendering for postfix completion items from this plugin,
  namely to support the ZIO icon and custom hint text. Unfortunately the regular Postfix templates API does not allow
  extending the item presentation easily.
 */
private[this] class ZPostfixLiveTemplateHack extends PostfixLiveTemplate {

  override def getLookupElements(
    file: PsiFile,
    editor: Editor,
    offset: Int
  ): util.Collection[_ <: CustomLiveTemplateLookupElement] = {
    if (!file.module.exists(_.hasZio)) return Collections.emptyList

    val result           = new util.HashSet[CustomLiveTemplateLookupElement]
    val callback         = new CustomTemplateCallback(editor, file)
    val parentDisposable = Disposer.newDisposable
    try {
      val provider = ZPostfixTemplateProvider
      ProgressManager.checkCanceled()
      val key = computeTemplateKeyWithoutContextChecking(callback)
      if (key != null && editor.getCaretModel.getCaretCount == 1) {
        PostfixTemplatesUtils.getAvailableTemplates(provider).asScala.foreach { template =>
          ProgressManager.checkCanceled()
          val icon = template match {
            case i: ZIcon => Some(i.getIcon(0))
            case _        => None
          }

          result.add(new PostfixTemplateLookupElement(this, template, template.getKey, provider, false) {
            override def renderElement(presentation: LookupElementPresentation): Unit = {
              icon.foreach(presentation.setIcon)
              super.renderElement(presentation)
            }
          })
        }
      }
    } finally Disposer.dispose(parentDisposable)

    result
  }

}
