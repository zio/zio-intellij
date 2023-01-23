package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.extensions.PsiClassExt
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import zio.intellij.inspections.zioLike

class YieldingZIOEffectInspection extends LocalInspectionTool {

  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case expr: ScFor =>
      expr.body match {
        case Some(e: ScBlock) =>
          e.exprs.lastOption match {
            case Some(body @ zioLike(_)) if hasGeneratorFromSameClass(expr, body) =>
              holder.registerProblem(body, YieldingZIOEffectInspection.message, ProblemHighlightType.WEAK_WARNING)
            case _ =>
          }
        case Some(body @ zioLike(_)) if hasGeneratorFromSameClass(expr, body) =>
          holder.registerProblem(body, YieldingZIOEffectInspection.message, ProblemHighlightType.WEAK_WARNING)
        case _ =>
      }
    case _ =>
  }

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
}

object YieldingZIOEffectInspection {

  @Nls
  val message =
    "Possibly mistaken wrapping of the result in a ZIO effect. Perhaps you meant to yield the result directly?"
}
