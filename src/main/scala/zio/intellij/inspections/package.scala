package zio.intellij

import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.codeInspection.collections.{isOfClassFrom, _}
import org.jetbrains.plugins.scala.lang.psi.api.base.patterns.{ScPattern, ScReferencePattern, ScWildcardPattern}
import org.jetbrains.plugins.scala.lang.psi.api.expr._
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDefinition
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScMember, ScObject, ScTemplateDefinition, ScTrait}
import zio.intellij.utils.TypeCheckUtils._
import zio.intellij.utils._
import zio.intellij.utils.types._

import scala.reflect.ClassTag

package object inspections {

  object collectionMethods {
    private[inspections] val `.map` = invocation("map").from(likeCollectionClasses)
  }

  object zioMethods {
    val `.*>` : Qualified                  = invocation("*>").from(zioLikePackages)
    val `.zipRight`: Qualified             = invocation("zipRight").from(zioLikePackages)
    val `.<*` : Qualified                  = invocation("<*").from(zioLikePackages)
    val `.zipLeft`: Qualified              = invocation("zipLeft").from(zioLikePackages)
    val `.as`: Qualified                   = invocation("as").from(zioLikePackages)
    val `.map`: Qualified                  = invocation("map").from(zioLikePackages)
    val `.flatten`: Qualified              = invocation("flatten").from(zioLikePackages)
    val `.flatMap`: Qualified              = invocation("flatMap").from(zioLikePackages)
    val `.flatMapError`: Qualified         = invocation("flatMapError").from(zioLikePackages)
    val `.mapError`: Qualified             = invocation("mapError").from(zioLikePackages)
    val `.mapBoth`: Qualified              = invocation("mapBoth").from(zioLikePackages)
    val `.orElse`: Qualified               = invocation("orElse").from(zioLikePackages)
    val `.orElseFail`: Qualified           = invocation("orElseFail").from(zioLikePackages)
    val `.orElseEither`: Qualified         = invocation("orElseEither").from(zioLikePackages)
    val `.<+>` : Qualified                 = invocation("<+>").from(zioLikePackages)
    val `.retryOrElseEither`: Qualified    = invocation("retryOrElseEither").from(zioLikePackages)
    val `.catchAll`: Qualified             = invocation("catchAll").from(zioLikePackages)
    val `.fold`: Qualified                 = invocation("fold").from(zioLikePackages)
    val `.fold_notStream`: Qualified       = invocation("fold").from(zioLike_notStream)
    val `.foldM_notStream`: Qualified      = invocation("foldM").from(zioLike_notStream)
    val `.foldCause`: Qualified            = invocation("foldCause").from(zioLikePackages)
    val `.foldCauseM`: Qualified           = invocation("foldCauseM").from(zioLikePackages)
    val `.foldTraceM`: Qualified           = invocation("foldTraceM").from(zioLikePackages)
    val `.tap_notStream`: Qualified        = invocation("tap").from(zioLike_notStream)
    val `.tapError`: Qualified             = invocation("tapError").from(zioLikePackages)
    val `.tapBoth`: Qualified              = invocation("tapBoth").from(zioLikePackages)
    val `.orDie`: Qualified                = invocation("orDie").from(zioLikePackages)
    val `.provide`: Qualified              = invocation("provide").from(zioSpecTypes ++ zioLikePackages)
    val `.provideSome`: Qualified          = invocation("provideSome").from(zioSpecTypes ++ zioLikePackages)
    val `.provideShared`: Qualified        = invocation("provideShared").from(zioTestPackage)
    val `.provideSomeShared`: Qualified    = invocation("provideSomeShared").from(zioTestPackage)
    val `.provideSomeLayer`: Qualified     = invocation("provideSomeLayer").from(zioLikePackages)
    val `.inject`: Qualified               = invocation("inject").from(zioMagicPackage)
    val `.injectSome`: Qualified           = invocation("injectSome").from(zioMagicPackage)
    val `.injectShared`: Qualified         = invocation("injectShared").from(zioMagicPackage)
    val `.injectSomeShared`: Qualified     = invocation("injectSomeShared").from(zioMagicPackage)
    val `.option`: Qualified               = invocation("option").from(zioLikePackages)
    val `.either`: Qualified               = invocation("either").from(zioLikePackages)
    val `.unit`: Qualified                 = invocation("unit").from(zioLikePackages)
    val `.fork`: Qualified                 = invocation("fork").from(zioLikePackages)
    val `.forkDaemon`: Qualified           = invocation("forkDaemon").from(zioLikePackages)
    val `.forkManaged`: Qualified          = invocation("forkManaged").from(zioLikePackages)
    val `.forkAs`: Qualified               = invocation("forkAs").from(zioLikePackages)
    val `.forkOn`: Qualified               = invocation("forkOn").from(zioLikePackages)
    val `.forkWithErrorHandler`: Qualified = invocation("forkWithErrorHandler").from(zioLikePackages)
  }

  object hasMethods {
    val zioHasLikeClasses: List[String] = List("zio.Has", "zio.Has._")

    private[inspections] val `.get` = invocation("get").from(zioHasLikeClasses)
  }

  object layerMethods {
    val `.build`: Qualified = invocation("build").from(zioLayerTypes)
  }

  object managedMethods {
    val `.use`: Qualified = invocation("use").from(managedTypes)
  }

  object assertMethods {
    val anything: Unqualified             = unqualified("anything").from(zioTestPackage)
    val equalTo: Unqualified              = unqualified("equalTo").from(zioTestPackage)
    val isGreaterThan: Unqualified        = unqualified("isGreaterThan").from(zioTestPackage)
    val isGreaterThanEqualTo: Unqualified = unqualified("isGreaterThanEqualTo").from(zioTestPackage)
    val isLessThan: Unqualified           = unqualified("isLessThan").from(zioTestPackage)
    val isLessThanEqualTo: Unqualified    = unqualified("isLessThanEqualTo").from(zioTestPackage)
    val isSome: Unqualified               = unqualified("isSome").from(zioTestPackage)
    val isLeft: Unqualified               = unqualified("isLeft").from(zioTestPackage)
    val isRight: Unqualified              = unqualified("isRight").from(zioTestPackage)
    val isEmpty: Unqualified              = unqualified("isEmpty").from(zioTestPackage)
    val isEmptyString: Unqualified        = unqualified("isEmptyString").from(zioTestPackage)
    val isNonEmpty: Unqualified           = unqualified("isNonEmpty").from(zioTestPackage)
    val isNegative: Unqualified           = unqualified("isNegative").from(zioTestPackage)
    val isPositive: Unqualified           = unqualified("isPositive").from(zioTestPackage)
    val isNaNDouble: Unqualified          = unqualified("isNaNDouble").from(zioTestPackage)
    val isNaNFloat: Unqualified           = unqualified("isNaNDouble").from(zioTestPackage)
    val isPosInfinityDouble: Unqualified  = unqualified("isPosInfinityDouble").from(zioTestPackage)
    val isPosInfinityFloat: Unqualified   = unqualified("isPosInfinityFloat").from(zioTestPackage)
    val isNegInfinityDouble: Unqualified  = unqualified("isNegInfinityDouble").from(zioTestPackage)
    val isNegInfinityFloat: Unqualified   = unqualified("isNegInfinityFloat").from(zioTestPackage)
    val isFiniteDouble: Unqualified       = unqualified("isFiniteDouble").from(zioTestPackage)
    val isFiniteFloat: Unqualified        = unqualified("isFiniteFloat").from(zioTestPackage)
    val isInfiniteDouble: Unqualified     = unqualified("isInfiniteDouble").from(zioTestPackage)
    val isInfiniteFloat: Unqualified      = unqualified("isInfiniteFloat").from(zioTestPackage)
    val isNull: Unqualified               = unqualified("isNull").from(zioTestPackage)
    val isTrue: Unqualified               = unqualified("isTrue").from(zioTestPackage)
    val isFalse: Unqualified              = unqualified("isFalse").from(zioTestPackage)
    val contains: Unqualified             = unqualified("contains").from(zioTestPackage)
    val containsString: Unqualified       = unqualified("containsString").from(zioTestPackage)
    val not: Unqualified                  = unqualified("not").from(zioTestPackage)
    val exists: Unqualified               = unqualified("exists").from(zioTestPackage)
    val startsWith: Unqualified           = unqualified("startsWith").from(zioTestPackage)
    val startsWithString: Unqualified     = unqualified("startsWithString").from(zioTestPackage)
    val endsWith: Unqualified             = unqualified("endsWith").from(zioTestPackage)
    val endsWithString: Unqualified       = unqualified("endsWithString").from(zioTestPackage)

    val assertFqn = new ZioType("test.CompileVariants")

    object AssertReferenceExtractor extends TraitMemberReferenceExtractor {
      override def types: Set[String] = Set(assertFqn.fqName)
    }

    val assertTrue = unqualified("assertTrue").from(zioTestPackage)
    val assert = new Curried2StaticMemberReference[ZioType](AssertReferenceExtractor, "assert") {
      override protected val typeCompanion: TypeCompanion[ZioType] = new TypeCompanion[ZioType] {
        override def values: List[ZioType] = List(assertFqn)
        override def defaultValue: ZioType = assertFqn
      }
    }

    val `&&` : Qualified = invocation("&&").from(zioTestPackage)

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
    extractor: MemberReferenceExtractor[ScTemplateDefinition],
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

  sealed abstract class MemberReferenceExtractor[+T <: ScTemplateDefinition: ClassTag] {
    def types: Set[String]

    private def findOverloaded(expr: ScReferenceExpression) =
      expr.multiResolveScala(incomplete = false) match {
        case result if result.isEmpty => None
        case result =>
          result.flatMap(_.fromType).distinct match {
            case Array(tpe) => fqnIfIsOfClassFrom(tpe, types.toSeq)
            case _          => None
          }
      }

    def unapply(ref: ScReferenceExpression): Option[String] =
      ref.resolve() match {
        case null                                                       => findOverloaded(ref)
        case t: ScTemplateDefinition if types.contains(t.qualifiedName) => Some(t.qualifiedName)
        case f: ScFunctionDefinition =>
          Option(f.containingClass).collect { case o: T => o.qualifiedName }.filter(types.contains)
        case _ => None
      }
  }

  sealed trait StaticMemberReferenceExtractor extends MemberReferenceExtractor[ScObject]
  sealed trait TraitMemberReferenceExtractor  extends MemberReferenceExtractor[ScTrait]

  object ZIOStaticMemberReferenceExtractor extends StaticMemberReferenceExtractor {
    override val types: Set[String] = zioTypes.toSet
  }

  object ZLayerStaticMemberReferenceExtractor extends StaticMemberReferenceExtractor {
    override val types: Set[String] = zioLayerTypes.toSet
  }

  class ReturnTypeReference(typeFQNs: Set[String]) {

    def unapply(expr: ScExpression): Option[ScExpression] =
      expr.`type`() match {
        case Right(t) if isOfClassFrom(t, typeFQNs.toSeq) => Some(expr)
        case _                                            => None
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
          case m: ScMember =>
            Option(m.containingClass).map(_.qualifiedName).flatMap(fqn => Option.when(typeFQNs.contains(fqn))(expr))
          case _ => None
        }
      case MethodRepr(_, Some(ref @ ScReferenceExpression(_)), None, Seq(_)) if isOfClassFrom(ref, typeFQNs.toSeq) =>
        Some(expr)
      case ref @ ScReferenceExpression(_) if isOfClassFrom(expr, typeFQNs.toSeq) => Some(ref)
      case _                                                                     => None
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
  val `ZIO.accessM`       = new ZIOStaticMemberReference("accessM")
  val `ZIO.service`       = new ZIOStaticMemberReference("service")
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

  object zioLike {

    def unapply(expr: ScExpression): Option[ScExpression] =
      Some(expr).filter(fromZioLike)
  }

  object zioSpec {

    def unapply(expr: ScExpression): Option[ScExpression] =
      Some(expr).filter(fromZioSpec)
  }

  val exitCodeSuccess = new ExitCode("success")
  val exitCodeFailure = new ExitCode("failure")

  class ExitCode(refName: String) {

    def unapply(expr: ScExpression): Boolean =
      expr match {
        case ref @ ScReferenceExpression(_) if ref.refName == refName =>
          ref.resolve() match {
            case p: ScReferencePattern =>
              Option(p.containingClass).map(_.qualifiedName).contains("zio.ExitCode")
            case _ => false
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
      case lambda(_, `ZIO.unit`(_, _)) => true
      case _                           => false
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

}
