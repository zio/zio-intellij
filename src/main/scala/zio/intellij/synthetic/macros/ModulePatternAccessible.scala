package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.{ScAnnotation, ScFieldId}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDeclaration
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, TermSignature}
import zio.intellij.inspections.fromZio

class ModulePatternAccessible extends SyntheticMembersInjector {

  private def members(sco: ScObject): Seq[String] = {
    val serviceTrait = sco.typeDefinitions.find(_.name == "Service")
    val methods      = serviceTrait.toSeq.flatMap(td => td.allMethods ++ td.allVals)

    object Field {
      def unapply(ts: TermSignature): Option[ScFieldId] =
        Some(ts.namedElement).collect {
          case fid: ScFieldId => fid
        }
    }

    methods.collect {
      case Field(fid) =>
        val mapOrFlatMap = if (fid.`type`().exists(fromZio)) "flatMap" else "map"
        s"val ${fid.name} = zio.ZIO.service[${sco.qualifiedName}.Service].$mapOrFlatMap(_.${fid.name})"
      case PhysicalMethodSignature(method: ScFunctionDeclaration, _) =>
        val name       = method.name
        val typeParams = method.typeParametersClause.map(_.getText).getOrElse("")
        val params     = method.paramClauses.getText
        val typeParameterApplication =
          method.typeParametersClause
            .map { tps =>
              tps.typeParameters
                .map(tp => tp.name)
                .mkString("[", ", ", "]")
            }
            .getOrElse("")
        val parameterApplication =
          method.paramClauses.clauses
            .map { clause =>
              clause.parameters
                .map { parameter =>
                  if (parameter.isVarArgs()) s"${parameter.name}: _*"
                  else parameter.name
                }
                .mkString("(", ", ", ")")
            }
            .mkString("")
        val mapOrFlatMap = if (method.returnType.exists(fromZio)) "flatMap" else "map"
        s"def $name$typeParams$params =" +
          s" zio.ZIO.service[${sco.qualifiedName}.Service]" +
          s".$mapOrFlatMap(_.$name$typeParameterApplication$parameterApplication)"
    }
  }

  private def findAccessibleMacroAnnotation(sco: ScObject): Option[ScAnnotation] =
    Option(sco.getAnnotation("zio.macros.accessible")).collect {
      case a: ScAnnotation => a
    }

  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    source match {
      case sco: ScObject =>
        val annotation = findAccessibleMacroAnnotation(sco)
        annotation.map(_ => members(sco)).getOrElse(Nil)
      case _ =>
        Nil
    }
}
