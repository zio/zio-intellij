package zio.intellij.postfix.templates

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.ScalaStringBasedPostfixTemplate
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.selector.AncestorSelector.SelectAllAncestors
import zio.intellij.intentions.ZIcon

final class ForeachTemplate
    extends ScalaStringBasedPostfixTemplate("traverse", "ZIO.foreach(xs)", SelectAllAncestors())
    with ZIcon {
  override def getTemplateString(element: PsiElement): String = "ZIO.foreach($expr$)($END$)"
}
