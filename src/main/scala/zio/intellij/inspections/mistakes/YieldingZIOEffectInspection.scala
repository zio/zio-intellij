package zio.intellij.inspections.mistakes

import com.intellij.codeInspection._
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import zio.intellij.inspections.zioLike
import zio.intellij.utils.fromSameClass

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
