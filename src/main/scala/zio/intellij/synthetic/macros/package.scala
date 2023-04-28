package zio.intellij.synthetic

import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import zio.intellij.synthetic.macros.utils.presentation._

package object macros {

  def typeParametersDefinition(tParams: Seq[ScTypeParam]): String =
    if (tParams.isEmpty) ""
    else presentationStringForScalaTypeParameters(tParams, showVariance = false)

  def typeParametersApplication(tParamsName: Seq[String]): String =
    if (tParamsName.isEmpty) ""
    else tParamsName.mkString("[", ", ", "]")

  def parametersDefinition(function: ScFunction): String =
    presentationStringForScalaParameters(function.paramClauses)

}
