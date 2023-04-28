package zio.intellij.synthetic.macros

import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScTypeAliasDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScTypeParam
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createTypeElementFromText
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.api.StdTypes
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType}
import zio.intellij.intentions.suggestions.SuggestTypeAlias
import zio.intellij.synthetic.macros.utils.presentation.defaultPresentationStringForScalaType
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._

abstract class ModulePatternAccessibleBase extends SyntheticMembersInjector {

  private val hasDesignator = "zio.Has"
  private val `???`         = "_root_.scala.Predef.???"

  private def has(tpe: String)(context: PsiElement): String = if (context.isZio1) s"$hasDesignator[$tpe]" else tpe

  protected val macroName: String

  protected def members(annotation: ScAnnotation, serviceTrait: ScTrait): Members

  protected abstract class Members(serviceTrait: ScTrait) {

    protected def modifyType(scType: ScType): ScType
    protected val typeArgsForService: Seq[String]
    protected val typeArgsForAccessors: Seq[ScTypeParam]

    final def apply(sco: ScObject): Seq[String] = {
      val serviceName =
        if (sco.typeDefinitions.exists(_.name == "Service")) s"${sco.qualifiedName}.Service"
        else sco.qualifiedName
      val aliasName = s"${sco.qualifiedName}.${sco.name}"
      val methods   = (serviceTrait.allMethods ++ serviceTrait.allVals).toSeq

      def withTypeParams(srv: String): String =
        s"$srv${typeParametersApplication(typeArgsForService)}"

      val serviceApplication = withTypeParams(serviceName)
      val aliasApplication   = withTypeParams(aliasName)

      val hasHasAlias = {
        val possibleAliasTpe =
          sco.aliases
            .find(_.name == sco.name)
            .collect { case ad: ScTypeAliasDefinition => ad }

        // direct call `createType("Has[Service]")` might throw a StackOverflow exception
        val hasServiceTpe = for {
          has        <- createType(hasDesignator, sco)
          service    <- findTypeDefByName(sco.getProject, serviceName)
          serviceTpe <- service.`type`().toOption
        } yield ScParameterizedType(has, Seq(serviceTpe))

        possibleAliasTpe.exists(alias => hasServiceTpe.exists(SuggestTypeAlias.equiv(alias, _).isDefined))
      }

      def returnType(typeInfo: TypeInfo) = {
        val serviceEnv = if (hasHasAlias) aliasApplication else has(serviceApplication)(sco)
        val currentEnv =
          if (typeInfo.rTypeParam.isAny) ""
          else s" with ${defaultPresentationStringForScalaType(typeInfo.rTypeParam)(sco)}"

        val r  = s"$serviceEnv$currentEnv"
        val ea = typeInfo.otherTypeParams.map(defaultPresentationStringForScalaType(_)(sco)).mkString(", ")

        s"${typeInfo.zioObject}[$r, $ea]"
      }

      methods.collect {
        case Field(field) =>
          val isPoly            = typeArgsForAccessors.nonEmpty
          val tpe               = modifyType(field.`type`().getOrAny)
          val typeInfo          = TypeInfo(tpe, field)
          val returnTypeAndBody = s"${returnType(typeInfo)} = ${`???`}"

          if (isPoly) s"def ${field.name}${typeParametersDefinition(typeArgsForAccessors)}: $returnTypeAndBody"
          else s"val ${field.name}: $returnTypeAndBody"

        case Method(method) =>
          val tpe                  = modifyType(method.returnType.getOrAny)
          val typeInfo             = TypeInfo(tpe, method)
          val typeParamsDefinition = typeParametersDefinition(typeArgsForAccessors ++ method.typeParameters)

          s"def ${method.name}$typeParamsDefinition${parametersDefinition(method)}: ${returnType(typeInfo)} = ${`???`}"
      }
    }
  }

  private def findAccessibleMacroAnnotation(sco: ScObject): Option[ScAnnotation] =
    Option(sco.fakeCompanionClassOrCompanionClass.getAnnotation(macroName)).collect {
      case a: ScAnnotation => a
    }

  private def findServiceTrait(sco: ScObject): Option[ScTrait] =
    sco.typeDefinitions.collectFirst {
      case tr: ScTrait if tr.name == "Service" => tr
    }.orElse(sco.fakeCompanionClassOrCompanionClass match {
      case typeDef: ScTrait => Some(typeDef)
      case _                => None
    })

  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    Some(source).collect { case sco: ScObject => sco }.flatMap { sco =>
      for {
        annotation   <- findAccessibleMacroAnnotation(sco)
        serviceTrait <- findServiceTrait(sco)
      } yield members(annotation, serviceTrait)(sco)
    }.getOrElse(Nil)

  case class TypeInfo private (
    zioObject: String,
    rTypeParam: ScType,
    otherTypeParams: List[ScType]
  )

  private object TypeInfo {
    def apply(tpe: ScType, ctx: PsiElement): TypeInfo = {
      val any = StdTypes.instance(tpe.projectContext).Any

      def zioTypeArgs(tpe: ScType): (ScType, List[ScType]) =
        extractAllTypeArguments(tpe).map(_.toList) match {
          case Some(r :: rest) => (r, rest)
          case _               => (any, List(any))
        }

      if (fromZio(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.ZIO",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else if (fromManaged(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.ZManaged",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else if (fromZioSink(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.stream.ZSink",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else if (fromZioStream(tpe)) {
        val (r, rest) = zioTypeArgs(tpe)
        new TypeInfo(
          zioObject = "zio.stream.ZStream",
          rTypeParam = r,
          otherTypeParams = rest
        )
      } else {
        val element   = createTypeElementFromText("Throwable", ctx)(ctx)
        val throwable = element.`type`().getOrAny
        new TypeInfo(
          zioObject = "zio.ZIO",
          rTypeParam = any,
          otherTypeParams = List(throwable, tpe)
        )
      }
    }
  }

  override def needsCompanionObject(source: ScTypeDefinition): Boolean =
    source.findAnnotationNoAliases(macroName) != null
}
