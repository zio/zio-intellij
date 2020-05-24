package zio.intellij.inspections.simplifications

import org.jetbrains.plugins.scala.codeInspection.collections.{Simplification, SimplificationType}
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScExpression, ScGenericCall, ScMethodCall}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.types.{AliasType, ScParameterizedType, ScType, TypePresentationContext}
import org.jetbrains.plugins.scala.lang.refactoring.ScTypePresentationExt
import zio.intellij.inspections._
import zio.intellij.inspections.hasMethods.`.get`

class SimplifyServiceInspection extends ZInspection(AccessGetSimplificationType)

object AccessGetSimplificationType extends SimplificationType {
  override def hint: String = "Replace with ZIO.service"

  private def replacement(accessExpr: ScExpression, accessTypeArg: Option[ScTypeElement] = None)(
    implicit ctx: TypePresentationContext = TypePresentationContext(accessExpr)
  ): Option[Simplification] = {
    def extractTypeArgument(tpe: ScType): Option[ScType] = tpe match {
      case parameterizedType: ScParameterizedType =>
        parameterizedType.typeArguments match {
          case Seq(typeArg) => Some(typeArg)
          case _            => None
        }
      case _ => None
    }

    val serviceTypeArg =
      accessTypeArg
        .flatMap(_.`type`().toOption)
        .flatMap { tpe =>
          if (!tpe.isAliasType) extractTypeArgument(tpe)
          else {
            tpe.aliasType.flatMap {
              case AliasType(typeDef: ScTypeAliasDefinition, _, _) =>
                typeDef.aliasedType.toOption.flatMap(extractTypeArgument)
              case _ => None
            }
          }
        }

    val simplification = replace(accessExpr)
      .withText(s"ZIO.service${serviceTypeArg.fold("")(t => s"[${t.codeText}]")}")
      .highlightFrom(accessExpr)

    Some(simplification)
  }

  override def getSimplification(expr: ScExpression): Option[Simplification] = expr match {
    // match both (_.get) and (h => h.get)
    case ScMethodCall(accessCall, Seq(_ `.get` () | lambda(_, Some(_ `.get` ())))) =>
      accessCall match {
        // ZIO.access(...)
        case `ZIO.access`(_) => replacement(expr)
        // ZIO.access[TypeParam](...)
        case ScGenericCall(`ZIO.access`(_), Seq(accessTypeArg)) =>
          replacement(expr, Some(accessTypeArg))
        case _ => None
      }
    case _ => None
  }
}
