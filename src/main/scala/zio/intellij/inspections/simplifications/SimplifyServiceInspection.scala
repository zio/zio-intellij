package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenericCall, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.types.TypePresentationContext
import zio.intellij.inspections._
import zio.intellij.inspections.hasMethods.`.get`

class SimplifyServiceInspection extends ZInspection(AccessGetSimplificationType)

object AccessGetSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.service"

  private def replacement(accessExpr: ScExpression, accessTypeParam: Option[ScTypeElement] = None)(
    implicit ctx: TypePresentationContext = TypePresentationContext(accessExpr)
  ): Option[Simplification] = {
    val serviceTypeParam =
      accessTypeParam
        .flatMap(_.`type`().toOption)
        .map(`type` => s"[${`type`.presentableText}]")
        .getOrElse("")

    Some(replace(accessExpr).withText(s"ZIO.service$serviceTypeParam").highlightFrom(accessExpr))
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    // match both (_.get) and (h => h.get)
    case ScMethodCall(accessCall, Seq(_ `.get` () | lambda(_, Some(_ `.get` ())))) =>
      accessCall match {
        // ZIO.access(...)
        case `ZIO.access`(_) => replacement(expr)
        // ZIO.access[TypeParam](...)
        case ScGenericCall(`ZIO.access`(_), Seq(accessTypeParam)) =>
          replacement(expr, Some(accessTypeParam))
        case _ => None
      }
    case _ => None
  }
}
