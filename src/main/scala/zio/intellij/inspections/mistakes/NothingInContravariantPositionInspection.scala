package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.AbstractRegisteredInspection
import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScParameterizedTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import zio.intellij.inspections.zioLikePackages

class NothingInContravariantPositionInspection extends AbstractRegisteredInspection {

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    (element.parent, element) match {
      case (
          Some(_: ScTypeAliasDefinition | _: ScFunctionDefinition | _: ScPatternDefinition),
          p @ ScParameterizedTypeElement(_, params)
          ) =>
        p.`type`().toOption match {
          case Some(tpe) if isOfClassFrom(tpe, zioLikePackages) =>
            tpe.extractDesignated(false) match {
              case Some(d: ScTypeParametersOwner) =>
                d.typeParameters.zip(params).collectFirst {
                  case (tp, elem)
                      if tp.isContravariant && elem.`type`().exists(_.isNothing) &&
                        // TODO hack, think of a better heuristic for this
                        // only prevent contravariant R (and R*) positions
                        tp.name.startsWith("R") =>
                    manager.createProblemDescriptor(
                      elem,
                      NothingInContravariantPositionInspection.message,
                      isOnTheFly,
                      Array.empty[LocalQuickFix],
                      ProblemHighlightType.WARNING
                    )
                }
              case _ => None
            }
          case _ => None

        }
      case _ => None
    }

}

object NothingInContravariantPositionInspection {
  val message = "Possibly erroneous use of Nothing in the R type parameter. Perhaps you meant to use Any?"
}
