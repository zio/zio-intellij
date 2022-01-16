package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.AbstractRegisteredInspection
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScSimpleTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.jetbrains.plugins.scala.lang.psi.types.ScalaTypePresentation.ObjectTypeSuffix

class IncorrectPreludeNewTypeAliasInspection extends AbstractRegisteredInspection {

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case s: ScTypeAliasDefinition =>
        s.aliasedTypeElement.collect {
          case sc @ ScSimpleTypeElement(ref) if sc.getParamTypeText.endsWith(ObjectTypeSuffix) =>
            Option(ref.resolve()).collect {
              case o: ScObject => o
            }.flatMap { self =>
              Option.when(ref.isReferenceTo(self) && isNewtypeOrSubtype(self))(
                manager.createProblemDescriptor(
                  sc,
                  s"Possibly mistaken use of '${self.name}.type' in the type alias, did you mean '${self.name}.Type' instead?",
                  isOnTheFly,
                  Array.empty[LocalQuickFix],
                  ProblemHighlightType.WARNING
                )
              )
            }
        }.flatten
      case _ => None
    }

  def isNewtypeOrSubtype(tpe: ScObject): Boolean =
    tpe.superTypes
      .map(_.canonicalText)
      .exists(c => c.startsWith("_root_.zio.prelude.Newtype") || c.startsWith("_root_.zio.prelude.Subtype"))

}
