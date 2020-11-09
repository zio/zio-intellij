package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.extensions.PsiClassExt
import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.base.types.{ScParameterizedTypeElement, ScTypeElement}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{ScParameter, ScTypeParam}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.{ScParameterizedType, ScType, TermSignature}
import zio.intellij.synthetic.macros.utils.presentation.defaultPresentationStringForScalaType
import zio.intellij.utils._

/**
 * @see https://github.com/zio/zio/blob/fa998a4ba8415bf9b6bb6b76db0eb42801ed4e5a/test/shared/src/main/scala-2.x/zio/test/mock/MockableMacro.scala
 */
class MockableInjector extends SyntheticMembersInjector {

  // extend annotated object with Mock
  override def injectSupers(source: ScTypeDefinition): Seq[String] =
    MockableInjector.findMacroAnnotationTypeElement(source) match {
      case Some((_, serviceClassName)) =>
        Seq(s"zio.test.mock.Mock[zio.Has[$serviceClassName]]")
      case _ => Seq.empty
    }

  // for each method in service generate a case object
  override def injectInners(source: ScTypeDefinition): Seq[String] =
    MockableInjector.findMacroAnnotationTypeElement(source) match {
      case Some((serviceElement, serviceClassName)) =>
        findTypeDefByName(serviceElement.getProject, serviceClassName).toSeq
          .flatMap(MockableInjector.makeTags)
      case _ => Nil
    }

  // inject val compose to compose service and proxy layers
  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    MockableInjector.findMacroAnnotationTypeElement(source) match {
      case Some((_, serviceClassName)) =>
        Seq(s"val compose: zio.URLayer[zio.Has[zio.test.mock.Proxy], zio.Has[$serviceClassName]] = ???")
      case _ => Seq.empty
    }

}

object MockableInjector {
  private[this] val mockableAnnotation = "zio.test.mock.mockable"

  def findMacroAnnotationTypeElement(source: ScTypeDefinition): Option[(ScTypeElement, String)] =
    Option(source.findAnnotationNoAliases(mockableAnnotation)).flatMap {
      case a: ScAnnotation =>
        a.typeElement match {
          case ScParameterizedTypeElement(_, Seq(serviceTypeElement)) =>
            for {
              serviceClassType <- serviceTypeElement.`type`().toOption
              serviceClass     <- serviceClassType.extractClass
            } yield (serviceTypeElement, serviceClass.qualifiedName)
          case _ => None
        }
      case _ => None
    }

  def makeTags(typeDefinition: ScTypeDefinition): List[String] =
    getMembers(typeDefinition).view.collect {
      case (name, info :: Nil) =>
        makeTag(name, info)
      case (name, infos) =>
        val tagName = name.capitalize
        val overloadedTags =
          sortOverloads(infos).zipWithIndex.map {
            case (info, idx) =>
              makeTag(s"_$idx", info)
          }
        s"""object $tagName {
           |  ${overloadedTags.mkString("\n  ")}
           |}""".stripMargin
    }.to(List)

  sealed trait Capability extends Product with Serializable

  object Capability {
    final case class Method(a: ScType)                       extends Capability
    final case class Effect(r: ScType, e: ScType, a: ScType) extends Capability
    final case class Stream(r: ScType, e: ScType, a: ScType) extends Capability
  }

  final case class MemberInfo(
    signature: TermSignature,
    capability: Capability,
    params: List[ScParameter],
    typeParams: List[ScTypeParam],
    i: ScType,
    e: ScType,
    a: ScType
  ) {

    private def isPoly(tpe: ScType): Boolean =
      tpe match {
        case parameterizedType: ScParameterizedType =>
          typeParams.exists(tp => parameterizedType.typeArguments.exists(_.canonicalText == tp.name))
        case _ =>
          typeParams.exists(_.name == tpe.canonicalText)
      }

    val polyI: Boolean = isPoly(i)
    val polyE: Boolean = isPoly(e)
    val polyA: Boolean = isPoly(a)
  }

  object MemberInfo {

    def apply(ts: TermSignature): Option[MemberInfo] = {

      def buildMemberInfo(
        optionalType: Option[ScType],
        optionalInput: Option[ScType],
        params: List[ScParameter] = Nil,
        typeParams: List[ScTypeParam] = Nil
      ) =
        for {
          tpe      <- optionalType
          resolved <- resolveAliases(tpe)
          fullName  = resolved.extractClass.fold(resolved.canonicalText)(_.qualifiedName)
          input    <- optionalInput
          (capability, eOpt, aOpt) = (extractTypeArguments(resolved), fullName) match {
                                       case (Some(Seq(r, e, a)), "zio.ZIO") =>
                                         (Capability.Effect(r, e, a), Some(e), Some(a))
                                       case (Some(Seq(r, e, a)), "zio.stream.ZStream") =>
                                         (Capability.Stream(r, e, a), Some(e), Some(a))
                                       case _ =>
                                         val eOpt = createType(text = "scala.Throwable", context = ts.namedElement)
                                         (Capability.Method(tpe), eOpt, Some(tpe))
                                     }
          e <- eOpt
          a <- aOpt
        } yield new MemberInfo(ts, capability, params, typeParams, input, e, a)

      ts match {
        case Method(method) =>
          val params     = method.paramClauses.clauses.flatMap(_.parameters).toList
          val typeParams = method.typeParametersClause.map(_.typeParameters.toList).getOrElse(Nil)
          val input =
            if (params.isEmpty) createType(text = "Unit", context = method)
            else
              params.flatMap(_.paramType).flatMap { t =>
                val optionalType = t.typeElement.`type`().toOption
                if (t.isRepeatedParameter) optionalType.map(tpe => s"scala.Seq[${tpe.canonicalText}]")
                else optionalType.map(_.canonicalText)
              } match {
                case Seq(p) => createType(text = p, context = method)
                case ps     => createType(text = ps.mkString("(", ", ", ")"), context = method)
              }

          buildMemberInfo(method.returnType.toOption, input, params, typeParams)
        case Field(field) =>
          buildMemberInfo(field.`type`().toOption, createType(text = "Unit", context = field))
        case _ => None
      }
    }
  }

  private def getMembers(typeDefinition: ScTypeDefinition): Map[String, List[MemberInfo]] =
    (typeDefinition.allVals ++ typeDefinition.allMethods).toList.filter {
      case Method(_) | Field(_) => true
      case _                    => false
    }
      .flatMap(MemberInfo.apply(_))
      .groupBy(_.signature.name)

  private def sortOverloads(infos: Seq[MemberInfo]): Seq[MemberInfo] = {
    import scala.math.Ordering.Implicits.seqOrdering
    import scala.collection.Seq

    infos.sortBy(_.signature)(Ordering[Seq[String]].on {
      case Method(method) =>
        for {
          clause      <- method.paramClauses.clauses
          param       <- clause.parameters
          paramType   <- param.paramType
          tpe         <- paramType.typeElement.`type`().toOption
          presentation = defaultPresentationStringForScalaType(tpe)
        } yield presentation
      case _ => Seq.empty
    })
  }

  private def makeTag(name: String, info: MemberInfo): String = {
    val tagName   = name.capitalize
    val (i, e, a) = (info.i, info.e, info.a)

    (info.capability, info.polyI, info.polyE, info.polyA) match {
      case (_: Capability.Method, false, false, false) =>
        s"case object $tagName extends Method[$i, $e, $a]"
      case (_: Capability.Method, true, false, false) =>
        s"case object $tagName extends Poly.Method.Input[$e, $a]"
      case (_: Capability.Method, false, true, false) =>
        s"case object $tagName extends Poly.Method.Error[$i, $a]"
      case (_: Capability.Method, false, false, true) =>
        s"case object $tagName extends Poly.Method.Output[$i, $e]"
      case (_: Capability.Method, true, true, false) =>
        s"case object $tagName extends Poly.Method.InputError[$a]"
      case (_: Capability.Method, true, false, true) =>
        s"case object $tagName extends Poly.Method.InputOutput[$e]"
      case (_: Capability.Method, false, true, true) =>
        s"case object $tagName extends Poly.Method.ErrorOutput[$i]"
      case (_: Capability.Method, true, true, true) =>
        s"case object $tagName extends Poly.Method.InputErrorOutput"

      case (_: Capability.Effect, false, false, false) =>
        s"case object $tagName extends Effect[$i, $e, $a]"
      case (_: Capability.Effect, true, false, false) =>
        s"case object $tagName extends Poly.Effect.Input[$e, $a]"
      case (_: Capability.Effect, false, true, false) =>
        s"case object $tagName extends Poly.Effect.Error[$i, $a]"
      case (_: Capability.Effect, false, false, true) =>
        s"case object $tagName extends Poly.Effect.Output[$i, $e]"
      case (_: Capability.Effect, true, true, false) =>
        s"case object $tagName extends Poly.Effect.InputError[$a]"
      case (_: Capability.Effect, true, false, true) =>
        s"case object $tagName extends Poly.Effect.InputOutput[$e]"
      case (_: Capability.Effect, false, true, true) =>
        s"case object $tagName extends Poly.Effect.ErrorOutput[$i]"
      case (_: Capability.Effect, true, true, true) =>
        s"case object $tagName extends Poly.Effect.InputErrorOutput"

      case (_: Capability.Stream, false, false, false) =>
        s"case object $tagName extends Stream[$i, $e, $a]"
      case (_: Capability.Stream, true, false, false) =>
        s"case object $tagName extends Poly.Stream.Input[$e, $a]"
      case (_: Capability.Stream, false, true, false) =>
        s"case object $tagName extends Poly.Stream.Error[$i, $a]"
      case (_: Capability.Stream, false, false, true) =>
        s"case object $tagName extends Poly.Stream.Output[$i, $e]"
      case (_: Capability.Stream, true, true, false) =>
        s"case object $tagName extends Poly.Stream.InputError[$a]"
      case (_: Capability.Stream, true, false, true) =>
        s"case object $tagName extends Poly.Stream.InputOutput[$e]"
      case (_: Capability.Stream, false, true, true) =>
        s"case object $tagName extends Poly.Stream.ErrorOutput[$i]"
      case (_: Capability.Stream, true, true, true) =>
        s"case object $tagName extends Poly.Stream.InputErrorOutput"
    }

  }

}
