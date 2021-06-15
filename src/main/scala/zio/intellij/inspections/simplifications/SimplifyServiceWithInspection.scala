package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.types.TypePresentationContext
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import zio.intellij.inspections._
import zio.intellij.inspections.hasMethods.`.get`
import zio.intellij.utils._
import zio.intellij.utils.types.ZioType

class SimplifyServiceWithInspection extends ZInspection(ServiceWithSimplificationType) {
  override protected def isAvailable(zioVersion: Version): Boolean = zioVersion >= Version.ZIO.`1.0.6`
}

object ServiceWithSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.serviceWith"

  private def replacement(
    zioType: ZioType,
    expr: ScExpression,
    ref: ScExpression,
    accessTypeArg: Option[ScTypeElement] = None
  )(implicit
    ctx: TypePresentationContext = TypePresentationContext(expr)
  ): Option[Simplification] = {
    val tpe = extractServiceTypeArgument(accessTypeArg)

    val refText = ref match {
      case f: ScFunctionExpr => LambdaUtils.lambdaToUnderscore(f)
      case _                 => ref.getText
    }

    val fnCall = refText.replace("_.get.", "_.")

    val replacement = replace(expr)
      .withText(s"${zioType.name}.serviceWith${tpe.fold("")(t => s"[${t.codeText}]")}($fnCall)")
      .highlightFrom(expr)

    Some(replacement)
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      case ScMethodCall(accessCall, Seq(InvokedExpression(ref))) =>
        accessCall match {
          // ZIO.accessM(...)
          case `ZIO.accessM`(zioType, _) =>
            replacement(zioType, expr, ref)
          // ZIO.accessM[TypeAlias](...)
          case ScGenericCall(`ZIO.accessM`(zioType, _), Seq(accessTypeArg)) =>
            replacement(zioType, expr, ref, Some(accessTypeArg))

          case _ => None
        }
      case _ => None
    }

  object InvokedExpression {

    /**
     * 1. _.get.foo is a reference expression, qualified with _.get (or x => x.get)
     * 2. _.get.foo(param) is a method call with an inner reference expr (1)
     * 3. x => x.get.foo is a function expression with an inner reference expr (1)
     */
    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case ref @ ScReferenceExpression.withQualifier(`.get`(_) | lambda(_, `.get`(_))) => Some(ref)
      case mc @ ScMethodCall(InvokedExpression(_), _)                                  => Some(mc)
      case fn @ ScFunctionExpr(_, Some(InvokedExpression(_)))                          => Some(fn)
      case _                                                                           => None
    }
  }
}
