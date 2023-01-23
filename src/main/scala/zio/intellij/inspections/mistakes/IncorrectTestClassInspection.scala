package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import com.intellij.execution.junit.JUnitUtil
import com.intellij.openapi.project.Project
import org.jetbrains.plugins.scala.annotator.template.isAbstract
import org.jetbrains.plugins.scala.codeInspection.{AbstractFixOnPsiElement, PsiElementVisitorSimple}
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenType.ObjectKeyword
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory
import org.jetbrains.plugins.scala.lang.psi.{ElementScope, ScalaPsiUtil}
import zio.intellij.inspections.mistakes.IncorrectTestClassInspection.ConvertToObject
import zio.intellij.testsupport.ZTestFramework.{ZIO1SpecFQN, ZIO2SpecFQN}

class IncorrectTestClassInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case c: ScClass if extendsZSpec(c) =>
      holder.registerProblem(
        c.targetToken,
        "ZIO Spec must be an 'object' instead of 'class'",
        ProblemHighlightType.GENERIC_ERROR,
        new ConvertToObject(c)
      )
    case _ =>
  }

  private def extendsZSpec(definition: ScClass) =
    if (isJUnitSpec(definition)) false
    else {
      val elementScope = ElementScope(definition.getProject)

      val cachedClass = elementScope.getCachedClass(ZIO1SpecFQN) orElse elementScope.getCachedClass(ZIO2SpecFQN)
      cachedClass.exists { c =>
        !isAbstract(definition) && ScalaPsiUtil.isInheritorDeep(definition, c)
      }
    }

  private def isJUnitSpec(clazz: ScClass) =
    JUnitUtil.isJUnit4TestClass(clazz)
}
object IncorrectTestClassInspection {
  final class ConvertToObject(param: ScClass) extends AbstractFixOnPsiElement("Replace 'class' with 'object'", param) {

    override protected def doApplyFix(c: ScClass)(implicit project: Project): Unit = {
      // borrowed from ConvertToObjectFix
      val classKeywordTextRange = c.targetToken.getTextRange

      val objectText = c.getText.patch(
        classKeywordTextRange.getStartOffset - c.getTextRange.getStartOffset,
        ObjectKeyword.text,
        classKeywordTextRange.getLength
      )

      val objectElement = ScalaPsiElementFactory.createObjectWithContext(objectText, c.getContext, c)
      c.replace(objectElement)
    }
  }

}
