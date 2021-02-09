package zio.intellij.postfix.templates

import com.intellij.codeInsight.template.Template
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.ScalaStringBasedPostfixTemplate
import org.jetbrains.plugins.scala.lang.completion.postfix.templates.selector.AncestorSelector.SelectAllAncestors
import zio.intellij.intentions.ZIcon

final class SucceedTemplate
    extends ScalaStringBasedPostfixTemplate("succeed", "ZIO.succeed(a)", SelectAllAncestors())
    with ZIcon {
  override def getTemplateString(element: PsiElement): String = "ZIO.succeed($expr$)"
}

final class FailTemplate
    extends ScalaStringBasedPostfixTemplate("fail", "ZIO.fail(a)", SelectAllAncestors())
    with ZIcon {
  override def getTemplateString(element: PsiElement): String = "ZIO.fail($expr$)"

  override def setVariables(template: Template, element: PsiElement): Unit = super.setVariables(template, element)
}
