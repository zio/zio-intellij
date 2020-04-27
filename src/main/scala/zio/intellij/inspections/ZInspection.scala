package zio.intellij.inspections

import javax.swing.JComponent
import org.jetbrains.plugins.scala.codeInspection.collections._
import org.jetbrains.plugins.scala.lang.psi.api.base.ScReference
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ ScExpression, ScMethodCall, ScReferenceExpression }
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter

abstract class ZInspection(simplifiers: SimplificationType*) extends OperationOnCollectionInspection {
  final override def getLikeCollectionClasses: Array[String] = Array("zio.ZIO")

  final override def createOptionsPanel(): JComponent = null // god help me

  final override def possibleSimplificationTypes: Array[SimplificationType] = simplifiers.toArray
}

object ZInspection {

  def simplifyFunctionCall(param: ScParameter, body: ScExpression): String =
    body match {
      case ref: ScReferenceExpression => s"_ => ${ref.getText}"
      case mc: ScMethodCall =>
        mc.argumentExpressions.toList match {
          case (ref: ScReference) :: Nil if ref.isReferenceTo(param) =>
            val invoked = mc.getEffectiveInvokedExpr
            if (invoked.textContains('.')) s"${param.getText} => ${mc.getText}" else invoked.getText
          case _ => s"${param.getText} => ${body.getText}"
        }
      case _ => s"${param.getText} => ${body.getText}"
    }
}
