package zio.intellij.postfix

import com.intellij.codeInsight.template.postfix.templates.{PostfixTemplate, PostfixTemplateProvider}
import com.intellij.openapi.editor.Editor
import com.intellij.psi.PsiFile
import zio.intellij.postfix.templates.{FailTemplate, ForeachTemplate, SucceedTemplate}

import java.util
import scala.jdk.CollectionConverters._

/*
  This provider is not registered in the plugin.xml because it's using a custom live template renderer to support icons.
  Do not register it as a template provider to avoid duplicate completion items!
 */
object ZPostfixTemplateProvider extends PostfixTemplateProvider {
  override def getTemplates: util.Set[PostfixTemplate] =
    Set[PostfixTemplate](
      new ForeachTemplate,
      new SucceedTemplate,
      new FailTemplate
    ).asJava

  override def isTerminalSymbol(currentChar: Char): Boolean = currentChar match {
    case '.' | '!' => true
    case _         => false
  }

  override def preExpand(file: PsiFile, editor: Editor): Unit = ()

  override def afterExpand(file: PsiFile, editor: Editor): Unit = ()

  override def preCheck(copyFile: PsiFile, realEditor: Editor, currentOffset: Int): PsiFile = copyFile
}
