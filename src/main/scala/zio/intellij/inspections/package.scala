package zio.intellij

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter

package object inspections {
  val zio = Array("zio.ZIO")

  def invocation(methodName: String) = new Qualified(methodName == _)

  private[inspections] val `.*>`         = invocation("*>").from(zio)
  private[inspections] val `.as`         = invocation("as").from(zio)
  private[inspections] val `.map`        = invocation("map").from(zio)
  private[inspections] val `.mapError`   = invocation("mapError").from(zio)
  private[inspections] val `.catchAll`   = invocation("catchAll").from(zio)
  private[inspections] val `.foldCause`  = invocation("foldCause").from(zio)
  private[inspections] val `.foldCauseM` = invocation("foldCauseM").from(zio)

  def fromZio(r: ScExpression): Boolean =
    isOfClassFrom(r, zio)

  object `ZIO.unit` {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ref @ ScReferenceExpression(_) if ref.refName == "unit" =>
        ref.resolve() match {
          case _: ScReferencePattern if fromZio(expr) => true
          case _                                      => false
        }
      case _ => false
    }
  }

  object `()` {
    def unapply(expr: ScExpression): Boolean = expr match {
      case _: ScUnitExpr => true
      case _             => false
    }
  }

  // todo deal with this nasty duplication.
  // I want to be able somehow select the matched extractor dynamically
  object `_ => ()` {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ScFunctionExpr(_, Some(result)) =>
        stripped(result) match {
          case `()`() => true
          case _      => false
        }
      case _ => false
    }
  }

  object `_ => x` {
    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case ScFunctionExpr(Seq(x), res @ Some(_)) if underscore(x) => res
      case _                                                      => None
    }

    // todo there must be a better way!
    def underscore(x: ScParameter): Boolean =
      x.isWildcard
  }

  object `_ => ZIO.unit` {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ScFunctionExpr(_, Some(result)) =>
        stripped(result) match {
          case `ZIO.unit`() => true
          case _            => false
        }
      case _ => false
    }
  }

}
