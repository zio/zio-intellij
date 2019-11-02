package zio.intellij

import org.jetbrains.plugins.scala.codeInspection.collections.Qualified
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ ScExpression, ScReferenceExpression }

package object inspections {
  val zio = Array("zio.ZIO")

  def invocation(methodName: String) = new Qualified(methodName == _)

  private[inspections] val `.*>` = invocation("*>").from(zio)

  object zioUnit {
    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case ref @ ScReferenceExpression(_) if ref.refName == "unit" =>
        ref.resolve() match {
          case m: ScReferencePattern if m.containingClass.qualifiedName == "zio.ZIOFunctions" => Some(ref)
          case _ => None
        }
      case _ => None
    }
  }
}
