package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import zio.intellij.inspections.fromZio

class ModulePatternAccessible extends SyntheticMembersInjector {

  private def members(sco: ScObject): Seq[String] = {
    val serviceTrait = sco.typeDefinitions.find(_.name == "Service")
    val methods      = serviceTrait.toSeq.flatMap(td => td.allMethods ++ td.allVals)

    methods.collect {
      case Field(fid) =>
        val mapOrFlatMap = if (fid.`type`().exists(fromZio)) "flatMap" else "map"
        s"val ${fid.name} = zio.ZIO.service[${sco.qualifiedName}.Service].$mapOrFlatMap(_.${fid.name})"
      case FunctionDeclaration(method) =>
        val name         = method.name
        val mapOrFlatMap = if (method.returnType.exists(fromZio)) "flatMap" else "map"
        s"def $name${typeParametersDefinition(method)}${parametersDefinition(method)} =" +
          s" zio.ZIO.service[${sco.qualifiedName}.Service]" +
          s".$mapOrFlatMap(_.$name${typeParametersApplication(method)}${parametersApplication(method)})"
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
