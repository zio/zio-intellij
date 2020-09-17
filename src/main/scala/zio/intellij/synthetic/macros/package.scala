package zio.intellij.synthetic

import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunction
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import zio.intellij.synthetic.macros.utils.presentation._

package object macros {

  def typeParametersDefinition(tpo: ScTypeParametersOwner, showVariance: Boolean): String =
    tpo.typeParametersClause.fold("")(presentationStringForScalaTypeParameters(_, showVariance))

  def typeParametersDefinition(tParams: Seq[ScTypeParam], showVariance: Boolean): String =
    if (tParams.isEmpty) ""
    else presentationStringForScalaTypeParameters(tParams, showVariance)

  def typeParametersApplication(tpo: ScTypeParametersOwner): String =
    tpo.typeParametersClause
      .fold("") { tps =>
        tps.typeParameters
          .map(_.name)
          .mkString("[", ", ", "]")
      }

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
