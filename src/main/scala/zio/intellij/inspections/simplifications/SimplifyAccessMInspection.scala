package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenericCall, ScMethodCall, ScReferenceExpression}
import org.jetbrains.plugins.scala.lang.psi.types.TypePresentationContext
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import zio.intellij.inspections._
import zio.intellij.inspections.hasMethods.`.get`
import zio.intellij.utils._
import zio.intellij.utils.types.ZioType

class SimplifyAccessMInspection extends ZInspection(ServiceWithSimplificationType) {
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
    val serviceTypeArg = for {
      arg           <- accessTypeArg
      tpe           <- arg.`type`().toOption
      baseType      <- resolveAliases(tpe)
      innerTypeArgs <- extractTypeArguments(baseType)
      if innerTypeArgs.size == 1 // should be exactly one argument
    } yield innerTypeArgs.head

    serviceTypeArg.map { tpe =>
      val fnCall = ref.getText.replace("_.get.", "_.")

      replace(expr)
        .withText(s"${zioType.name}.serviceWith[${tpe.codeText}])($fnCall)")
        .highlightFrom(expr)
    }
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      // match both (_.get.foo) and (h => h.get.foo), also (_.get.foo(2))
      case ScMethodCall(accessCall, Seq(InvokedExpression(ref))) =>
        accessCall match {
          // ZIO.accessM(...)
          case `ZIO.accessM`(zioType, _) => replacement(zioType, expr, ref)
          // ZIO.accessM[TypeAlias](...)
          case ScGenericCall(`ZIO.accessM`(zioType, _), Seq(accessTypeArg)) =>
            replacement(zioType, expr, ref, Some(accessTypeArg))
          case _ => None
        }
      case _ => None
    }

  object InvokedExpression {
    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case ref @ ScReferenceExpression.withQualifier(`.get`(_) | lambda(_, `.get`(_))) => Some(ref)
      case mc @ ScMethodCall(InvokedExpression(_), _)                                  => Some(mc)
      case _                                                                           => None
    }
  }
}
