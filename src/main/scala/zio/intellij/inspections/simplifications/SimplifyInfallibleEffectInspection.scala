package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{
  invocationText,
  Qualified,
  Simplification,
  SimplificationType
}
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import zio.intellij.inspections._
import zio.intellij.inspections.zioMethods._
import zio.intellij.utils.StringUtils.ScExpressionExt
import zio.intellij.utils.TypeCheckUtils.isInfallibleEffect
import zio.intellij.utils._
import zio.intellij.utils.types.ZioType

class SimplifyErrorModificationInspection
    extends ZInspection(
      BimapErrorModificationSimplificationType,
      TapBothErrorModificationSimplificationType,
      FoldErrorModificationSimplificationType,
      FoldMErrorModificationSimplificationType,
      FoldTraceMErrorModificationSimplificationType
    )

class SimplifyErrorRecoveryInspection
    extends ZInspection(
      OptionErrorRecoverySimplificationType,
      EitherErrorRecoverySimplificationType,
      OrElseEitherErrorRecoverySimplificationType,
      OrElseEitherAliasErrorRecoverySimplificationType,
      RetryOrElseEitherErrorRecoverySimplificationType
    )

class SimplifyErrorSeparationInspection
    extends ZInspection(
      PartitionErrorSeparationSimplificationType,
      PartitionParErrorSeparationSimplificationType,
      PartitionParNErrorSeparationSimplificationType,
      ValidateErrorSeparationSimplificationType,
      ValidateParErrorSeparationSimplificationType,
      ValidateFirstErrorSeparationSimplificationType,
      ValidateFirstParErrorSeparationSimplificationType
    )

sealed abstract class BaseInfallibleEffectSimplificationType extends SimplificationType {
  final protected def isInfallibleFunc(func: ScExpression): Boolean =
    func match {
      // zio.flatMap(foo)
      case ScReferenceExpression(f: ScFunction) => isInfallibleEffect(f.returnType)
      /* workaround for
          val foo: Any => UIO[Any] = ???
          zio.flatMap(foo)
       */
      case ScReferenceExpression(Typeable(funcType)) =>
        extractTypeArguments(funcType).flatMap(_.lastOption).exists(isInfallibleEffect)
      // zio.flatMap(el => foo(el))
      case lambda(_, res) => isInfallibleEffect(res)
      // zio.flatMap(foo(_))
      case expr => isInfallibleEffect(expr.getNonValueType(fromUnderscore = true))
    }
}

sealed abstract class BaseErrorModificationSimplificationType(qual: Qualified, methodName: String)
    extends BaseInfallibleEffectSimplificationType {

  val hint = s"Replace with .$methodName"

  private def replacement(expr: ScExpression, zio: ScExpression, g: ScExpression): Simplification =
    replace(expr).withText(invocationText(zio, methodName, g)).highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual(zio, _, g) if isInfallibleEffect(zio) => Some(replacement(expr, zio, g))
      case _                                          => None
    }

}

object BimapErrorModificationSimplificationType   extends BaseErrorModificationSimplificationType(`.bimap`, "map")
object TapBothErrorModificationSimplificationType extends BaseErrorModificationSimplificationType(`.tapBoth`, "tap")

object FoldErrorModificationSimplificationType  extends BaseErrorModificationSimplificationType(`.fold`, "map")
object FoldMErrorModificationSimplificationType extends BaseErrorModificationSimplificationType(`.foldM`, "flatMap")
object FoldTraceMErrorModificationSimplificationType
    extends BaseErrorModificationSimplificationType(`.foldTraceM`, "flatMap")

sealed abstract class BaseErrorRecoverySimplificationType(qual: Qualified, methodStr: String)
    extends BaseInfallibleEffectSimplificationType {

  val hint = s"Replace with .map($methodStr)"

  private def replacement(expr: ScExpression, zio: ScExpression): Option[Simplification] =
    createExpression(methodStr, expr).map(m => replace(expr).withText(invocationText(zio, "map", m)).highlightAll)

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual(zio, _*) if isInfallibleEffect(zio) => replacement(expr, zio)
      case _                                        => None
    }

}

object OptionErrorRecoverySimplificationType extends BaseErrorRecoverySimplificationType(`.option`, "Some(_)")
object EitherErrorRecoverySimplificationType extends BaseErrorRecoverySimplificationType(`.either`, "Right(_)")
// `Left(_)` is not a mistake here. Props to consistent API.
object OrElseEitherErrorRecoverySimplificationType
    extends BaseErrorRecoverySimplificationType(`.orElseEither`, "Left(_)")
object OrElseEitherAliasErrorRecoverySimplificationType extends BaseErrorRecoverySimplificationType(`.<+>`, "Left(_)")
object RetryOrElseEitherErrorRecoverySimplificationType
    extends BaseErrorRecoverySimplificationType(`.retryOrElseEither`, "Right(_)")

sealed abstract class BaseErrorSeparationSimplificationType(
  qual: ZIOCurried2StaticMemberReference,
  methodName: String
) extends BaseInfallibleEffectSimplificationType {

  val hint = s"Replace with ZIO.$methodName"

  protected def replacement(
    zioType: ZioType,
    expr: ScExpression,
    iterable: ScExpression,
    func: ScExpression
  ): Simplification =
    replace(expr).withText(s"${zioType.name}.$methodName${iterable.getWrappedText}${func.getWrappedText}").highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual(zioType, iterable, func) if isInfallibleFunc(func) => Some(replacement(zioType, expr, iterable, func))
      case _                                                       => None
    }

}

object PartitionErrorSeparationSimplificationType
    extends BaseErrorSeparationSimplificationType(`ZIO.partition`, "foreach")
object PartitionParErrorSeparationSimplificationType
    extends BaseErrorSeparationSimplificationType(`ZIO.partitionPar`, "foreachPar")
object ValidateErrorSeparationSimplificationType
    extends BaseErrorSeparationSimplificationType(`ZIO.validate`, "foreach")
object ValidateParErrorSeparationSimplificationType
    extends BaseErrorSeparationSimplificationType(`ZIO.validatePar`, "foreachPar")
object ValidateFirstErrorSeparationSimplificationType
    extends BaseErrorSeparationSimplificationType(`ZIO.validateFirst`, "foreach")
object ValidateFirstParErrorSeparationSimplificationType
    extends BaseErrorSeparationSimplificationType(`ZIO.validateFirstPar`, "foreachPar")

object PartitionParNErrorSeparationSimplificationType extends BaseInfallibleEffectSimplificationType {

  private val qual: ZIOCurried3StaticMemberReference = `ZIO.partitionParN`

  private val methodName = "foreachParN"

  val hint = s"Replace with ZIO.$methodName"

  protected def replacement(
    zioType: ZioType,
    expr: ScExpression,
    n: ScExpression,
    iterable: ScExpression,
    func: ScExpression
  ): Simplification =
    replace(expr)
      .withText(s"${zioType.name}.$methodName${n.getWrappedText}${iterable.getWrappedText}${func.getWrappedText}")
      .highlightAll

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case qual(zioType, n, iterable, func) if isInfallibleFunc(func) =>
        Some(replacement(zioType, expr, n, iterable, func))
      case _ => None
    }

}
