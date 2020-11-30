package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}
import zio.intellij.synthetic.macros.utils.presentation.defaultPresentationStringForScalaType
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._

abstract class ModulePatternAccessibleBase extends SyntheticMembersInjector {

  private val hasDesignator = "zio.Has"

  protected val macroName: String

  protected def members(annotation: ScAnnotation, serviceTrait: ScTrait): Members

  protected abstract class Members(serviceTrait: ScTrait) {

    protected def modifyType(scType: ScType): ScType
    protected val typeArgsForService: Seq[String]
    protected val typeArgsForAccessors: Seq[ScTypeParam]

    final def apply(sco: ScObject): Seq[String] = {
      val serviceName = s"${sco.qualifiedName}.Service"
      val aliasName   = s"${sco.qualifiedName}.${sco.name}"
      val methods     = (serviceTrait.allMethods ++ serviceTrait.allVals).toSeq

      def withTypeParams(srv: String): String =
        s"$srv${typeParametersApplication(typeArgsForService)}"

      val serviceApplication = withTypeParams(serviceName)
      val aliasApplication   = withTypeParams(aliasName)

      val hasHasAlias = {
        val possibleAliasTpe =
          sco.aliases
            .find(_.name == sco.name)
            .collect { case ad: ScTypeAliasDefinition => ad }
            .flatMap(_.aliasedType.toOption)

        // direct call `createType("Has[Service]")` might throw a StackOverflow exception
        val hasServiceTpe = for {
          has        <- createType(hasDesignator, sco)
          service    <- findTypeDefByName(sco.getProject, serviceName)
          serviceTpe <- service.`type`.toOption
        } yield ScParameterizedType(has, Seq(serviceTpe))

        possibleAliasTpe.exists(alias => hasServiceTpe.exists(_.equiv(alias)))
      }

      val requiredEnv =
        if (hasHasAlias) aliasApplication
        else s"$hasDesignator[$serviceApplication]"

      def returnType(typeInfo: TypeInfo) =
        s"${typeInfo.zioObject}[$requiredEnv" +
          s"${if (typeInfo.rTypeParam.isAny) ""
          else s" with ${defaultPresentationStringForScalaType(typeInfo.rTypeParam)}"}, " +
          s"${typeInfo.otherTypeParams.map(defaultPresentationStringForScalaType).mkString(", ")}]"

      methods.collect {
        case Field(field) =>
          val isPoly   = typeArgsForAccessors.nonEmpty
          val tpe      = modifyType(field.`type`().getOrAny)
          val typeInfo = TypeInfo(tpe)
          val returnTypeAndBody = s"${returnType(typeInfo)} = " +
            s"${typeInfo.zioObject}.${typeInfo.accessMethod}(_.get[$serviceApplication].${field.name})"

          if (isPoly)
            s"def ${field.name}${typeParametersDefinition(typeArgsForAccessors, showVariance = false)}: $returnTypeAndBody"
          else s"val ${field.name}: $returnTypeAndBody"

        case Method(method) =>
          val tpe      = modifyType(method.returnType.getOrAny)
          val typeInfo = TypeInfo(tpe)
          val typeParamsDefinition =
            typeParametersDefinition(
              typeArgsForAccessors ++ method.typeParameters,
              showVariance = false
            )

          s"def ${method.name}$typeParamsDefinition${parametersDefinition(method)}: ${returnType(typeInfo)} = " +
            s"${typeInfo.zioObject}.${typeInfo.accessMethod}(_.get[$serviceApplication]" +
            s".${method.name}${typeParametersApplication(method)}${parametersApplication(method)})"
      }
    }
  }

  private def findAccessibleMacroAnnotation(sco: ScObject): Option[ScAnnotation] =
    Option(sco.getAnnotation(macroName)).collect {
      case a: ScAnnotation => a
    }

  private def findServiceTrait(sco: ScObject): Option[ScTrait] =
    sco.typeDefinitions.collectFirst {
      case tr: ScTrait if tr.name == "Service" => tr
    }

  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    Some(source).collect { case sco: ScObject => sco }.flatMap { sco =>
      for {
        annotation   <- findAccessibleMacroAnnotation(sco)
        serviceTrait <- findServiceTrait(sco)
      } yield members(annotation, serviceTrait)(sco)
    }.getOrElse(Nil)

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
