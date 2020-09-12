package zio.intellij.synthetic

import org.jetbrains.plugins.scala.lang.psi.api.base.ScFieldId
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.ScReferencePattern
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration, ScFunctionDefinition}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.{ScNamedElement, ScTypeParametersOwner}
import org.jetbrains.plugins.scala.lang.psi.types.result.Typeable
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, TermSignature}
import zio.intellij.synthetic.macros.utils.presentation._

package object macros {

  object Field {

    def unapply(ts: TermSignature): Option[ScNamedElement with Typeable] =
      Some(ts.namedElement).collect {
        case fid: ScFieldId          => fid
        case ref: ScReferencePattern => ref
      }
  }

  object Method {

    def unapply(ts: TermSignature): Option[ScFunction] =
      ts match {
        case PhysicalMethodSignature(method: ScFunctionDeclaration, _) => Some(method)
        case PhysicalMethodSignature(method: ScFunctionDefinition, _)  => Some(method)
        case _                                                         => None
      }
  }

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
