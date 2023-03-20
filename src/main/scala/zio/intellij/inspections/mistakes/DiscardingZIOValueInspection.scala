package zio.intellij.inspections.mistakes

import com.intellij.codeInspection.{LocalInspectionTool, ProblemHighlightType, ProblemsHolder}
import org.jetbrains.annotations.Nls
import org.jetbrains.plugins.scala.codeInspection.PsiElementVisitorSimple
import org.jetbrains.plugins.scala.codeInspection.collections.{invocationText, Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScFunctionExpr, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.ParameterizedType
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import zio.intellij.inspections.mistakes.DiscardingZIOValueInspectionBase.ReturnType
import zio.intellij.inspections.streamMethods.`.runDrain`
import zio.intellij.inspections.zioMethods.`.map`
import zio.intellij.inspections.{lambda, ZInspection}
import zio.intellij.utils.StringUtils.ScExpressionExt
import zio.intellij.utils.TypeCheckUtils.{`ZStream[R, E, O]`, fromZioLike}
import zio.intellij.utils.fromSameClass

// inspections with suggestions
class DiscardingZIOValueSmartInspection extends ZInspection(DiscardingZIOValueMapToFlatMapSmartInspection)
// general things that we might not know how to fix automatically
class DiscardingZIOValueInspection extends LocalInspectionTool with DiscardingZIOValueInspectionBase {
  override def buildVisitor(holder: ProblemsHolder, isOnTheFly: Boolean): PsiElementVisitorSimple = {
    case qual `.map` f =>
      returnType(f).foreach {
        case returnType if zioLikeReplacedWithUnit(returnType) && !exprIsOfSameClass(qual, returnType.actual) =>
          holder.registerProblem(f, DiscardingZIOValueInspection.mapDiscardMsg, ProblemHighlightType.WEAK_WARNING)
        case _ =>
      }
    case expr @ `.runDrain`(Typeable(`ZStream[R, E, O]`(_, _, out))) if fromZioLike(out) =>
      holder.registerProblem(expr, DiscardingZIOValueInspection.runDrainDiscardMsg, ProblemHighlightType.WEAK_WARNING)
    case _ =>
  }
}

object DiscardingZIOValueInspection {

  @Nls
  val mapDiscardMsg = "Possibly mistaken value discarding"

  @Nls
  val runDrainDiscardMsg = "Possibly mistaken value discarding by .runDrain"

}

object DiscardingZIOValueMapToFlatMapSmartInspection extends SimplificationType with DiscardingZIOValueInspectionBase {

  private val replacement = "flatMap"

  override def hint: String = s"Possibly mistaken value discarding. Perhaps you meant to use .$replacement?"

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual `.map` f =>
        returnType(f).collect {
          case returnType if zioLikeReplacedWithUnit(returnType) && exprIsOfSameClass(qual, returnType.actual) =>
            replacement(expr, qual, f)
        }
      case _ => None
    }

  protected def replacement(expr: ScExpression, qual: ScExpression, body: ScExpression): Simplification =
    replace(expr).withText(invocationText(qual, s"$replacement${body.getWrappedText}"))

}

trait DiscardingZIOValueInspectionBase {

  protected def zioLikeReplacedWithUnit(returnType: ReturnType): Boolean =
    fromZioLike(returnType.actual) && returnType.expected.isUnit

  protected def exprIsOfSameClass(expr: ScExpression, tpe: ScType): Boolean =
    expr.`type`().exists(fromSameClass(_, tpe))

  protected def returnType(func: ScExpression): Option[ReturnType] =
    func match {
      // zio.flatMap(foo)
      case ScReferenceExpression(f: ScFunction) =>
        (f.returnType, func.expectedType(fromUnderscore = false)) match {
          case (Right(actual), Some(ParameterizedType(_, typeArgs))) => typeArgs.lastOption.map(ReturnType(actual, _))
          case _                                                     => None
        }
      // zio.flatMap(el => foo(el))
      case lambda(_, res) =>
        (res.getTypeIgnoreBaseType, func) match {
          case (Right(actual), ScFunctionExpr(_, Some(Typeable(expected)))) => Some(ReturnType(actual, expected))
          case _                                                            => None
        }
      // zio.flatMap(foo(_))
      case expr =>
        (expr.getNonValueType(fromUnderscore = true), func.expectedType(fromUnderscore = true)) match {
          case (Right(actual), Some(expected)) => Some(ReturnType(actual, expected))
          case _                               => None
        }
    }

}

object DiscardingZIOValueInspectionBase {
  final case class ReturnType(actual: ScType, expected: ScType)
}
