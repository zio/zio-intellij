package zio.intellij.synthetic

import org.jetbrains.plugins.scala.lang.psi.api.base.ScFieldId
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScFunction, ScFunctionDeclaration}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, TermSignature}
import zio.intellij.synthetic.macros.utils.presentation._

package object macros {

  object Field {

    def unapply(ts: TermSignature): Option[ScFieldId] =
      Some(ts.namedElement).collect {
        case fid: ScFieldId => fid
      }
  }

  object FunctionDeclaration {

    def unapply(ts: TermSignature): Option[ScFunctionDeclaration] = ts match {
      case PhysicalMethodSignature(method: ScFunctionDeclaration, _) => Some(method)
      case _                                                         => None
    }
  }

  def typeParametersDefinition(tpo: ScTypeParametersOwner): String =
    tpo.typeParametersClause.fold("")(presentationStringForScalaTypeParameters)

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
    function.paramClauses.clauses
      .map { clause =>
        clause.parameters
          .map { parameter =>
            if (parameter.isVarArgs()) s"${parameter.name}: _*"
            else parameter.name
          }
          .mkString("(", ", ", ")")
      }
      .mkString("")

}
