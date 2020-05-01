package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.codeInspection.AbstractRegisteredInspection
import org.jetbrains.plugins.scala.extensions.PsiClassExt
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.util.IntentionAvailabilityChecker
import zio.intellij.inspections.zioRef

class YieldingZIOEffectInspection extends AbstractRegisteredInspection {

  private def createDescriptor(
    expr: ScExpression
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): ProblemDescriptor =
    manager.createProblemDescriptor(
      expr,
      YieldingZIOEffectInspection.message,
      isOnTheFly,
      Array.empty[LocalQuickFix],
      ProblemHighlightType.WEAK_WARNING
    )

  private def typeName(e: ScExpression): Option[String] =
    e.`type`()
      .toOption
      .flatMap(_.tryExtractDesignatorSingleton.extractClass)
      .map(_.qualifiedName)

  private def fromSameClass(e1: ScExpression, e2: ScExpression): Boolean =
    (typeName(e1), typeName(e2)) match {
      case (Some(t1), Some(t2)) => t1 == t2
      case _                    => false
    }

  private def hasGeneratorFromSameClass(forExpr: ScFor, expr: ScExpression): Boolean =
    forExpr.enumerators.toList
      .flatMap(_.generators)
      .flatMap(_.expr)
      .exists(fromSameClass(_, expr))

  override protected def problemDescriptor(
    element: PsiElement,
    maybeQuickFix: Option[LocalQuickFix],
    descriptionTemplate: String,
    highlightType: ProblemHighlightType
  )(implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] =
    element match {
      case expr: ScFor if IntentionAvailabilityChecker.checkInspection(this, expr.getParent) =>
        expr.body match {
          case Some(body @ zioRef(_, _)) if hasGeneratorFromSameClass(expr, body) =>
            Some(createDescriptor(body))
          case Some(e: ScBlock) =>
            e.exprs.lastOption match {
              case Some(body @ zioRef(_, _)) if hasGeneratorFromSameClass(expr, body) =>
                Some(createDescriptor(body))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
}

object YieldingZIOEffectInspection {
  val message = "Possibly mistaken yielding of a ZIO effect. Perhaps you meant to yield the result instead?"
}
