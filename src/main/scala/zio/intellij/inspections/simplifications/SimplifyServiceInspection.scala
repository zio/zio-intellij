package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenericCall, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.types.TypePresentationContext
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import zio.intellij.inspections._
import zio.intellij.inspections.hasMethods.`.get`
import zio.intellij.utils.types.ZioType
import zio.intellij.utils.{extractTypeArguments, resolveAliases}

class SimplifyServiceInspection extends ZInspection(AccessGetSimplificationType)

object AccessGetSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.service"

  private def replacement(zioType: ZioType, accessExpr: ScExpression, accessTypeArg: Option[ScTypeElement] = None)(
    implicit ctx: TypePresentationContext = TypePresentationContext(accessExpr)
  ): Option[Simplification] = {
    val serviceTypeArg = for {
      arg           <- accessTypeArg
      tpe           <- arg.`type`().toOption
      baseType      <- resolveAliases(tpe)
      innerTypeArgs <- extractTypeArguments(baseType)
      if innerTypeArgs.size == 1 // should be exactly one argument
    } yield innerTypeArgs.head

    val simplification = replace(accessExpr)
      .withText(s"${zioType.name}.service${serviceTypeArg.fold("")(t => s"[${t.codeText}]")}")
      .highlightFrom(accessExpr)

    Some(simplification)
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] =
    expr match {
      // match both (_.get) and (h => h.get)
      case ScMethodCall(accessCall, Seq(_ `.get` () | lambda(_, _ `.get` ()))) =>
        accessCall match {
          // ZIO.access(...)
          case `ZIO.access`(zioType, _) => replacement(zioType, expr)
          // ZIO.access[TypeParam](...)
          case ScGenericCall(`ZIO.access`(zioType, _), Seq(accessTypeArg)) =>
            replacement(zioType, expr, Some(accessTypeArg))
          case _ => None
        }
      case _ => None
    }
}
