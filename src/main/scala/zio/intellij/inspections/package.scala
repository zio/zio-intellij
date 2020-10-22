package zio.intellij

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.codeInspection.collections.{isOfClassFrom, _}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScPattern, ScReferencePattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScTemplateDefinition}
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._
import zio.intellij.utils.types._

package object inspections {

  object collectionMethods {
    private[inspections] val `.map` = invocation("map").from(likeCollectionClasses)
  }

  object zioMethods {
    val `.*>` : Qualified               = invocation("*>").from(zioLikePackages)
    val `.as`: Qualified                = invocation("as").from(zioLikePackages)
    val `.map`: Qualified               = invocation("map").from(zioLikePackages)
    val `.flatMap`: Qualified           = invocation("flatMap").from(zioLikePackages)
    val `.flatMapError`: Qualified      = invocation("flatMapError").from(zioLikePackages)
    val `.mapError`: Qualified          = invocation("mapError").from(zioLikePackages)
    val `.bimap`: Qualified             = invocation("bimap").from(zioLikePackages)
    val `.orElse`: Qualified            = invocation("orElse").from(zioLikePackages)
    val `.orElseFail`: Qualified        = invocation("orElseFail").from(zioLikePackages)
    val `.orElseEither`: Qualified      = invocation("orElseEither").from(zioLikePackages)
    val `.<+>` : Qualified              = invocation("<+>").from(zioLikePackages)
    val `.retryOrElseEither`: Qualified = invocation("retryOrElseEither").from(zioLikePackages)
    val `.catchAll`: Qualified          = invocation("catchAll").from(zioLikePackages)
    val `.fold`: Qualified              = invocation("fold").from(zioLikePackages)
    val `.foldM`: Qualified             = invocation("foldM").from(zioLikePackages)
    val `.foldCause`: Qualified         = invocation("foldCause").from(zioLikePackages)
    val `.foldCauseM`: Qualified        = invocation("foldCauseM").from(zioLikePackages)
    val `.foldTraceM`: Qualified        = invocation("foldTraceM").from(zioLikePackages)
    val `.tap`: Qualified               = invocation("tap").from(zioLikePackages)
    val `.tapError`: Qualified          = invocation("tapError").from(zioLikePackages)
    val `.tapBoth`: Qualified           = invocation("tapBoth").from(zioLikePackages)
    val `.orDie`: Qualified             = invocation("orDie").from(zioLikePackages)
    val `.provide`: Qualified           = invocation("provide").from(zioLikePackages)
    val `.option`: Qualified            = invocation("option").from(zioLikePackages)
    val `.either`: Qualified            = invocation("either").from(zioLikePackages)

    val `.fork`: Qualified                 = invocation("fork").from(zioLikePackages)
    val `.forkDaemon`: Qualified           = invocation("forkDaemon").from(zioLikePackages)
    val `.forkManaged`: Qualified          = invocation("forkManaged").from(zioLikePackages)
    val `.forkAs`: Qualified               = invocation("forkAs").from(zioLikePackages)
    val `.forkOn`: Qualified               = invocation("forkOn").from(zioLikePackages)
    val `.forkWithErrorHandler`: Qualified = invocation("forkWithErrorHandler").from(zioLikePackages)

    private[inspections] val `assert` = unqualified("assert").from(zioLikePackages)
  }

  object hasMethods {
    val zioHasLikeClasses: Array[String] = Array("zio.Has", "zio.Has._")

    private[inspections] val `.get` = invocation("get").from(zioHasLikeClasses)
  }

  object layerMethods {
    val `.build`: Qualified = invocation("build").from(zioLayerTypes)
  }

  object managedMethods {
    val `.use`: Qualified = invocation("use").from(managedTypes)
  }

  def invocation(methodName: String)  = new Qualified(methodName == _)
  def unqualified(methodName: String) = new Unqualified(methodName == _)

  object methodExtractors {

    object uncurry1 {

      def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] =
        expr match {
          case MethodRepr(_, _, Some(ref), Seq(e)) => Some((ref, e))
          case _                                   => None
        }
    }

    object uncurry2 {

      def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression, ScExpression)] =
        expr match {
          case MethodRepr(_, Some(uncurry1(ref, first)), _, Seq(second)) => Some(ref, first, second)
          case _                                                         => None
        }
    }

    object uncurry3 {

      def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression, ScExpression, ScExpression)] =
        expr match {
          case MethodRepr(_, Some(uncurry2(ref, first, second)), _, Seq(third)) => Some(ref, first, second, third)
          case _                                                                => None
        }
    }
  }

  import methodExtractors._

  sealed abstract class BaseStaticMemberReference[T <: Type](refName: String) {
    protected def typeCompanion: TypeCompanion[T]

    protected def matchesRefName(ref: ScReferenceExpression): Boolean =
      if (ref.refName == refName) true
      else
        ref.resolve() match {
          // handles the 'apply' case when called with ZIO(x)
          case n: ScNamedElement if n.name == refName => true
          case _                                      => false
        }
  }

  sealed abstract class StaticMemberReference[T <: Type](extractor: StaticMemberReferenceExtractor, refName: String)
      extends BaseStaticMemberReference[T](refName) {

    def unapply(expr: ScExpression): Option[(T, ScExpression)] =
      expr match {
        case ref @ ScReferenceExpression(_) if matchesRefName(ref) =>
          ref.smartQualifier match {
            case Some(extractor(fqn)) => Some((typeCompanion.fromFQName(fqn), expr))
            case _                    => None
          }
        case uncurry1(ref, first) if matchesRefName(ref) =>
          ref match {
            case extractor(fqn) => Some((typeCompanion.fromFQName(fqn), first))
            case _              => None
          }
        case _ => None
      }
  }

  sealed abstract class Curried2StaticMemberReference[T <: Type](
    extractor: StaticMemberReferenceExtractor,
    refName: String
  ) extends BaseStaticMemberReference[T](refName) {

    def unapply(expr: ScExpression): Option[(T, ScExpression, ScExpression)] = expr match {
      case uncurry2(ref, first, second) if matchesRefName(ref) =>
        ref match {
          case extractor(fqn) => Some((typeCompanion.fromFQName(fqn), first, second))
          case _              => None
        }
      case _ => None
    }
  }

  sealed abstract class Curried3StaticMemberReference[T <: Type](
    extractor: StaticMemberReferenceExtractor,
    refName: String
  ) extends BaseStaticMemberReference[T](refName) {

    def unapply(expr: ScExpression): Option[(T, ScExpression, ScExpression, ScExpression)] = expr match {
      case uncurry3(ref, first, second, third) if matchesRefName(ref) =>
        ref match {
          case extractor(fqn) => Some((typeCompanion.fromFQName(fqn), first, second, third))
          case _              => None
        }
      case _ => None
    }

  }

  final class ZIOStaticMemberReference(refName: String)
      extends StaticMemberReference[ZioType](ZIOStaticMemberReferenceExtractor, refName) {
    override protected val typeCompanion: TypeCompanion[ZioType] = ZioTypes
  }

  final class ZIOCurried2StaticMemberReference(refName: String)
      extends Curried2StaticMemberReference[ZioType](ZIOStaticMemberReferenceExtractor, refName) {
    override protected val typeCompanion: TypeCompanion[ZioType] = ZioTypes
  }

  final class ZIOCurried3StaticMemberReference(refName: String)
      extends Curried3StaticMemberReference[ZioType](ZIOStaticMemberReferenceExtractor, refName) {
    override protected val typeCompanion: TypeCompanion[ZioType] = ZioTypes
  }

  final class ZLayerStaticMemberReference(refName: String)
      extends StaticMemberReference[ZLayerType](ZLayerStaticMemberReferenceExtractor, refName) {
    override protected val typeCompanion: TypeCompanion[ZLayerType] = ZLayerTypes
  }

  sealed trait StaticMemberReferenceExtractor {
    def types: Set[String]

    private def findOverloaded(expr: ScReferenceExpression) =
      expr.multiResolveScala(incomplete = false) match {
        case result if result.isEmpty => None
        case result =>
          result.flatMap(_.fromType).distinct match {
            case Array(tpe) => fqnIfIsOfClassFrom(tpe, types.toArray)
            case _          => None
          }
      }

    def unapply(ref: ScReferenceExpression): Option[String] =
      ref.resolve() match {
        case null                                                       => findOverloaded(ref)
        case t: ScTemplateDefinition if types.contains(t.qualifiedName) => Some(t.qualifiedName)
        case f: ScFunctionDefinition =>
          Option(f.containingClass).map(_.qualifiedName).filter(types.contains)
        case _ => None
      }
  }

  object ZIOStaticMemberReferenceExtractor extends StaticMemberReferenceExtractor {
    override val types: Set[String] = zioTypes.toSet
  }

  object ZLayerStaticMemberReferenceExtractor extends StaticMemberReferenceExtractor {
    override val types: Set[String] = zioLayerTypes.toSet
  }

  class ReturnTypeReference(typeFQNs: Set[String]) {

    def unapply(expr: ScExpression): Option[ScExpression] =
      expr.`type`() match {
        case Right(t) if isOfClassFrom(t, typeFQNs.toArray) => Some(expr)
        case _                                              => None
      }
  }

  val scalaFuture = new ReturnTypeReference(Set("scala.concurrent.Future"))
  val scalaTry    = new ReturnTypeReference(Set("scala.util.Try", "scala.util.Success", "scala.util.Failure"))
  val scalaOption = new ReturnTypeReference(Set("scala.Option", "scala.Some", "scala.None"))
  val scalaEither = new ReturnTypeReference(Set("scala.util.Either", "scala.util.Left", "scala.util.Right"))

  class TypeReference(typeFQNs: Set[String]) {

    def unapply(expr: ScExpression): Option[ScExpression] = expr match {
      case uncurry1(ref, _) =>
        ref.resolve() match {
          case m: ScMember if typeFQNs.contains(m.containingClass.qualifiedName) => Some(expr)
          case _                                                                 => None
        }
      case MethodRepr(_, Some(ref @ ScReferenceExpression(_)), None, Seq(_)) if isOfClassFrom(ref, typeFQNs.toArray) =>
        Some(expr)
      case ref @ ScReferenceExpression(_) if isOfClassFrom(expr, typeFQNs.toArray) => Some(ref)
      case _                                                                       => None
    }
  }

  val scalaLeft  = new TypeReference(Set("scala.util.Left"))
  val scalaRight = new TypeReference(Set("scala.util.Right"))

  val `ZIO.apply`         = new ZIOStaticMemberReference("apply")
  val `ZIO.unit`          = new ZIOStaticMemberReference("unit")
  val `ZIO.succeed`       = new ZIOStaticMemberReference("succeed")
  val `ZIO.fail`          = new ZIOStaticMemberReference("fail")
  val `ZIO.collectAll`    = new ZIOStaticMemberReference("collectAll")
  val `ZIO.collectAllPar` = new ZIOStaticMemberReference("collectAllPar")
  val `ZIO.sleep`         = new ZIOStaticMemberReference("sleep")
  val `ZIO.effect`        = new ZIOStaticMemberReference("effect")
  val `ZIO.effectTotal`   = new ZIOStaticMemberReference("effectTotal")
  val `ZIO.access`        = new ZIOStaticMemberReference("access")
  val `ZIO.forkAll`       = new ZIOStaticMemberReference("forkAll")
  val `ZIO.forkAll_`      = new ZIOStaticMemberReference("forkAll_")

  val `ZIO.collectAllParN`   = new ZIOCurried2StaticMemberReference("collectAllParN")
  val `ZIO.foreach`          = new ZIOCurried2StaticMemberReference("foreach")
  val `ZIO.foreachPar`       = new ZIOCurried2StaticMemberReference("foreachPar")
  val `ZIO.partition`        = new ZIOCurried2StaticMemberReference("partition")
  val `ZIO.partitionPar`     = new ZIOCurried2StaticMemberReference("partitionPar")
  val `ZIO.validate`         = new ZIOCurried2StaticMemberReference("validate")
  val `ZIO.validatePar`      = new ZIOCurried2StaticMemberReference("validatePar")
  val `ZIO.validateFirst`    = new ZIOCurried2StaticMemberReference("validateFirst")
  val `ZIO.validateFirstPar` = new ZIOCurried2StaticMemberReference("validateFirstPar")

  val `ZIO.foreachParN`   = new ZIOCurried3StaticMemberReference("foreachParN")
  val `ZIO.partitionParN` = new ZIOCurried3StaticMemberReference("partitionParN")

  val `ZLayer.fromEffect`     = new ZLayerStaticMemberReference("fromEffect")
  val `ZLayer.fromEffectMany` = new ZLayerStaticMemberReference("fromEffectMany")

  object unit {

    def unapply(expr: ScExpression): Boolean =
      expr match {
        case _: ScUnitExpr => true
        case _             => false
      }
  }

  object zioRef {

    def unapply(expr: ScExpression): Option[(ScReferenceExpression, ScExpression)] = expr match {
      case ref @ ScReferenceExpression(_) =>
        ref.resolve() match {
          case _: ScReferencePattern | _: ScFunctionDefinition if fromZioLike(expr) => Some((ref, expr))
          case _                                                                    => None
        }
      case uncurry1(ref, e) if fromZioLike(expr) => Some((ref, e))
      // multiple argument lists
      case uncurry2(ref, _, _) if fromZioLike(expr)    => Some((ref, expr))
      case uncurry3(ref, _, _, _) if fromZioLike(expr) => Some((ref, expr))
      case _                                           => None
    }
  }

  val exitCodeSuccess = new ExitCode("success")
  val exitCodeFailure = new ExitCode("failure")

  class ExitCode(refName: String) {

    def unapply(expr: ScExpression): Boolean =
      expr match {
        case ref @ ScReferenceExpression(_) if ref.refName == refName =>
          ref.resolve() match {
            case p: ScReferencePattern if p.containingClass.qualifiedName == "zio.ExitCode" => true
            case _                                                                          => false
          }
        case _ => false
      }
  }

  object lambda {

    def unapply(expr: ScExpression): Option[(Seq[ScParameter], ScExpression)] = expr match {
      case ScFunctionExpr(params @ Seq(_), Some(res)) =>
        Some(params, stripped(res))
      case _ => None
    }
  }

  // for comprehension `x <- xx` generator syntax
  object generator {

    def unapply(expr: ScGenerator): Option[(ScPattern, Option[ScExpression])] =
      (expr.pattern, expr.expr) match {
        case (x, res @ Some(_)) =>
          Some((x, res.map(stripped)))
        case _ => None
      }
  }

  object guard {
    def unapply(expr: ScGuard): Option[ScExpression] = expr.expr
  }

  object `_ => x` {

    def unapply(expr: ScExpression): Option[ScExpression] =
      expr match {
        case lambda(Seq(x), res) if underscore(x) => Some(res)
        case _                                    => None
      }

    // todo there must be a better way!
    def underscore(x: ScParameter): Boolean =
      x.isWildcard
  }

  // todo deal with this nasty duplication.
  // I want to be able somehow select the matched extractor dynamically
  object `_ => ()` {

    def unapply(expr: ScExpression): Boolean =
      expr match {
        case lambda(_, unit()) => true
        case _                 => false
      }
  }

  object `_ => ZIO.unit` {

    def unapply(expr: ScExpression): Boolean = expr match {
      case lambda(_, `ZIO.unit`(_)) => true
      case _                        => false
    }
  }

  object `_ <- x` {

    def unapply(expr: ScGenerator): Option[ScExpression] = expr match {
      case generator(_: ScWildcardPattern, expr) => expr
      case _                                     => None
    }
  }

  object IsDeprecated {

    def unapply(expr: ScExpression): Option[PsiAnnotation] =
      expr match {
        case ScMethodCall(ref: ScReferenceExpression, _) =>
          ref.resolve() match {
            case fn: ScFunctionDefinition if fn.isDeprecated => Some(fn.findAnnotation("scala.deprecated"))
            case _                                           => None
          }
        case _ => None
      }
  }

  object `ZIO[R, E, A]` {
    private def unapplyInner(tpe: ScType): Option[(ScType, ScType, ScType)] =
      resolveAliases(tpe.tryExtractDesignatorSingleton).flatMap(extractTypeArguments).flatMap {
        case Seq(r, e, a) => Some(r, e, a)
        case _            => None
      }

    def unapply(tpe: ScType): Option[(ScType, ScType, ScType)] =
      if (fromZio(tpe)) unapplyInner(tpe) else None
  }

  object `URIO[R, A]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZIO[R, E, A]`(r, e, a) if e.isNothing => Some(r, a)
        case _                                      => None
      }
  }
}
