package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSimpleTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.ScalaTypePresentation.ObjectTypeSuffix
import org.jetbrains.plugins.scala.lang.psi.{ElementScope, ScalaPsiUtil}

class IncorrectPreludeNewTypeAliasInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case s: ScTypeAliasDefinition =>
      s.aliasedTypeElement.collect {
        case sc @ ScSimpleTypeElement(ref) if sc.getParamTypeText.endsWith(ObjectTypeSuffix) =>
          Option(ref.resolve()).collect {
            case o: ScObject if isNewtype(o) => o
          }.foreach { self =>
            if (ref.isReferenceTo(self)) {
              holder.registerProblem(
                sc,
                s"Possibly mistaken use of '${self.name}.type' in the type alias, did you mean '${self.name}.Type' instead?",
                ProblemHighlightType.WARNING
              )
            }
          }
      }
    case _ =>
  }

  def isNewtype(tpe: ScObject): Boolean = {
    val elementScope = ElementScope(tpe.getProject)

    // covers Subtype as well, since Subtype extends Newtype in prelude
    val newtype = elementScope.getCachedClass("zio.prelude.Newtype")
    newtype.exists { nt =>
      ScalaPsiUtil.isInheritorDeep(tpe, nt)
    }
  }
}
