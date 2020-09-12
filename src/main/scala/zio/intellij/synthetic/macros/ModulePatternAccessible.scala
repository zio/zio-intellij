package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import zio.intellij.inspections.{fromManaged, fromZio}

class ModulePatternAccessible extends SyntheticMembersInjector {

  private def members(sco: ScObject): Seq[String] = {
    val serviceTrait       = sco.typeDefinitions.find(_.name == "Service")
    val methods            = serviceTrait.toSeq.flatMap(td => td.allMethods ++ td.allVals)
    val serviceApplication = s"${sco.qualifiedName}.Service${serviceTrait.fold("")(typeParametersApplication)}"

    def mapOrFlatMap(tpe: ScType): String = if (fromZio(tpe) || fromManaged(tpe)) "flatMap" else "map"
    def zioObject(tpe: ScType): String    = if (fromManaged(tpe)) "ZManaged" else "ZIO"

    methods.collect {
      case Field(field) =>
        val isPoly = serviceTrait.exists(_.typeParameters.nonEmpty)
        val tpe    = field.`type`().getOrAny
        val body   = s"zio.${zioObject(tpe)}.service[$serviceApplication].${mapOrFlatMap(tpe)}(_.${field.name})"

        if (isPoly)
          s"def ${field.name}${serviceTrait.fold("")(typeParametersDefinition(_, showVariance = false))} = $body"
        else s"val ${field.name} = $body"

      case Method(method) =>
        val tpe = method.returnType.getOrAny
        val typeParamsDefinition =
          typeParametersDefinition(
            serviceTrait.toSeq.flatMap(_.typeParameters) ++ method.typeParameters,
            showVariance = false
          )

        s"def ${method.name}$typeParamsDefinition${parametersDefinition(method)} = " +
          s"zio.${zioObject(tpe)}.service[$serviceApplication]" +
          s".${mapOrFlatMap(tpe)}(_.${method.name}${typeParametersApplication(method)}${parametersApplication(method)})"
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
