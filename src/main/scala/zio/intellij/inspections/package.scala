package zio.intellij

import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr._

package object inspections {
  val zio = Array("zio.ZIO")

  def invocation(methodName: String) = new Qualified(methodName == _)

  private[inspections] val `.*>`  = invocation("*>").from(zio)
  private[inspections] val `.as`  = invocation("as").from(zio)
  private[inspections] val `.map` = invocation("map").from(zio)

  object zioUnit {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ref @ ScReferenceExpression(_) if ref.refName == "unit" =>
        ref.resolve() match {
          case m: ScReferencePattern if m.containingClass.qualifiedName == "zio.ZIOFunctions" => true
          case _                                                                              => false
        }
      case _ => false
    }
  }

  object unitLiteral {
    def unapply(expr: ScExpression): Boolean = expr match {
      case _: ScUnitExpr => true
      case _             => false
    }
  }

  object returnsUnit {
    def unapply(expr: ScExpression): Boolean = expr match {
      case ScFunctionExpr(_, Some(result)) =>
        stripped(result) match {
          case unitLiteral() => true
          case _             => false
        }
      case _ => false
    }
  }

}
