package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.{ ScAnnotation, ScFieldId }
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDeclaration
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ ScObject, ScTypeDefinition }
import org.jetbrains.plugins.scala.lang.psi.impl.base.ScLiteralImpl
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.{ PhysicalMethodSignature, TermSignature }

class ModulePatternAccessible extends SyntheticMembersInjector {

  private def annotationFirstParam(scAnnotation: ScAnnotation): Option[String] =
    scAnnotation.annotationExpr.getAnnotationParameters.collectFirst {
      case sl: ScLiteralImpl => sl.getValue()
    }

  private def helperObjectExtension(annotation: ScAnnotation, sco: ScObject): Seq[String] =
    annotationFirstParam(annotation)
      .map(name => s"object $name { ${helperObjectBody(sco)} }")
      .toSeq

  private def helperObjectBody(sco: ScObject): String = {
    val serviceTrait = sco.typeDefinitions.find(_.name == "Service")
    val methods      = serviceTrait.toSeq.flatMap(td => td.allMethods ++ td.allVals)

    object Field {
      def unapply(ts: TermSignature): Option[ScFieldId] =
        Some(ts.namedElement).collect {
          case fid: ScFieldId => fid
        }
    }

    val signatures = methods.collect {
      case Field(fid) =>
        s"val ${fid.name} = zio.ZIO.access[zio.Has[${sco.qualifiedName}.Service]](_.get).flatMap(_.${fid.name})"
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
        s"def $name$typeParams$params =" +
          s" zio.ZIO.access[zio.Has[${sco.qualifiedName}.Service]](_.get)" +
          s".flatMap(_.$name$typeParameterApplication$parameterApplication)"
    }

    signatures.mkString("\n")
  }

  private def findAccessibleMacroAnnotation(sco: ScObject): Option[ScAnnotation] =
    Option(sco.getAnnotation("zio.macros.annotation.accessible")).collect {
      case a: ScAnnotation => a
    }

  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    source match {
      case sco: ScObject =>
        val annotation = findAccessibleMacroAnnotation(sco)
        annotation.map(a => helperObjectExtension(a, sco)).getOrElse(Nil)
      case _ =>
        Nil
    }
}
