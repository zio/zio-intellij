package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import com.intellij.psi.PsiElement
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScParameterizedTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import zio.intellij.utils.TypeCheckUtils.zioLikePackages

class NothingInContravariantPositionInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple =
    (element: PsiElement) =>
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
                      holder.registerProblem(
                        elem,
                        NothingInContravariantPositionInspection.message,
                        ProblemHighlightType.WARNING
                      )
                  }
                case _ =>
              }
            case _ =>

          }
        case _ =>
      }
}

object NothingInContravariantPositionInspection {
  @Nls
  val message = "Possibly erroneous use of Nothing in the R type parameter. Perhaps you meant to use Any?"
}
