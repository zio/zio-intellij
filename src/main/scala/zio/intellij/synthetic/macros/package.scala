package zio.intellij.synthetic

import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import zio.intellij.synthetic.macros.utils.presentation._

package object macros {

  def typeParametersDefinition(tParams: Seq[ScTypeParam], showVariance: Boolean): String =
    if (tParams.isEmpty) ""
    else presentationStringForScalaTypeParameters(tParams, showVariance)

  def typeParametersApplication(tpo: ScTypeParametersOwner): String =
    typeParametersApplication(tpo.typeParameters.map(_.name))

  def typeParametersApplication(tParamsName: Seq[String]): String =
    if (tParamsName.isEmpty) ""
    else tParamsName.mkString("[", ", ", "]")

  def parametersDefinition(function: ScFunction): String =
    presentationStringForScalaParameters(function.paramClauses)

  def parametersApplication(function: ScFunction): String =
    function.paramClauses.clauses.map { clause =>
      clause.parameters.map { parameter =>
        if (parameter.isVarArgs()) s"${parameter.name}: _*"
        else parameter.name
      }
        .mkString("(", ", ", ")")
    }
      .mkString("")

}
