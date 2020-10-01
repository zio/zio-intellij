package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import zio.intellij.synthetic.macros.utils.presentation.defaultPresentationStringForScalaType
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._

class ModulePatternAccessible extends SyntheticMembersInjector {

  private def members(sco: ScObject): Seq[String] = {
    val serviceTrait       = sco.typeDefinitions.find(_.name == "Service")
    val methods            = serviceTrait.toSeq.flatMap(td => td.allMethods ++ td.allVals)
    val serviceApplication = s"${sco.qualifiedName}.Service${serviceTrait.fold("")(typeParametersApplication)}"

    def returnType(typeInfo: TypeInfo) =
      s"${typeInfo.zioObject}[zio.Has[$serviceApplication]" +
        s"${if (typeInfo.rTypeParam.isAny) ""
        else s" with ${defaultPresentationStringForScalaType(typeInfo.rTypeParam)}"}, " +
        s"${typeInfo.otherTypeParams.map(defaultPresentationStringForScalaType).mkString(", ")}]"

    methods.collect {
      case Field(field) =>
        val isPoly   = serviceTrait.exists(_.typeParameters.nonEmpty)
        val tpe      = field.`type`().getOrAny
        val typeInfo = TypeInfo(tpe)
        val returnTypeAndBody = s"${returnType(typeInfo)} = " +
          s"${typeInfo.zioObject}.${typeInfo.accessMethod}(_.get[$serviceApplication].${field.name})"

        if (isPoly)
          s"def ${field.name}${serviceTrait.fold("")(typeParametersDefinition(_, showVariance = false))}: $returnTypeAndBody"
        else s"val ${field.name}: $returnTypeAndBody"

      case Method(method) =>
        val tpe      = method.returnType.getOrAny
        val typeInfo = TypeInfo(tpe)
        val typeParamsDefinition =
          typeParametersDefinition(
            serviceTrait.toSeq.flatMap(_.typeParameters) ++ method.typeParameters,
            showVariance = false
          )

        s"def ${method.name}$typeParamsDefinition${parametersDefinition(method)}: ${returnType(typeInfo)} = " +
          s"${typeInfo.zioObject}.${typeInfo.accessMethod}(_.get[$serviceApplication]" +
          s".${method.name}${typeParametersApplication(method)}${parametersApplication(method)})"
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

  private final case class TypeInfo(
    zioObject: String,
    accessMethod: String,
    rTypeParam: ScType,
    otherTypeParams: List[ScType]
  )

  private object TypeInfo {
    def apply(tpe: ScType): TypeInfo = {
      val any = StdTypes.instance(tpe.projectContext).Any

      def zioTypeArgs(tpe: ScType): (ScType, List[ScType]) =
        resolveAliases(tpe).flatMap(extractTypeArguments).map(_.toList) match {
          case Some(r :: rest) => (r, rest)
          case _               => (any, List(any))
        }

      if (fromZio(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.ZIO",
          accessMethod = "accessM",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else if (fromManaged(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.ZManaged",
          accessMethod = "accessManaged",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else if (fromZioSink(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.stream.ZSink",
          accessMethod = "accessSink",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else if (fromZioStream(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.stream.ZStream",
          accessMethod = "accessStream",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else {
        val throwable = createTypeElementFromText("Throwable")(tpe.projectContext).`type`().getOrAny
        new TypeInfo(
          zioObject = "zio.ZIO",
          accessMethod = "access",
          rTypeParam = any,
          otherTypeParams = List(throwable, tpe)
        )
      }
    }
  }
}
