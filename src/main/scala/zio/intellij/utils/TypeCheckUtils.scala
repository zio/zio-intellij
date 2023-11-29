package zio.intellij.utils

import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import zio.intellij.utils.types.{ZLayerTypes, ZioTypes}

object TypeCheckUtils {

  val zioCompanionSpecificTypes      = List("zio.ZIOCompanionVersionSpecific", "zio.ZIOCompanionPlatformSpecific")
  val zioLayerCompanionSpecificTypes = List("zio.ZLayerCompanionVersionSpecific")

  val zioTypes        = ZioTypes.values.map(_.fqName) :+ "zio.ZIOPlatformSpecific" :+ "zio.ZIOVersionSpecific"
  val zioLayerTypes   = ZLayerTypes.values.map(_.fqName)
  val zioSinkTypes    = List("zio.stream.ZSink")
  val zioStreamTypes  = List("zio.stream.ZStream")
  val managedTypes    = List("zio.ZManaged")
  val extraTypes      = List("zio.Fiber", "zio.ZQueue", "zio.ZRef", "zio.ZRefM", "zio.ZQuery")
  val zioTestAsserts  = List("zio.test.Assertion._", "zio.test.BoolAlgebra", "zio.test.BoolAlgebraM", "zio.test.Assert")
  val zioTestPackage  = List("zio.test._")
  val zioMagicPackage = List("zio.magic._")
  val zioSpecTypes    = List("zio.test.Spec", "zio.test.SpecVersionSpecific")
  // ZStreams API signatures sometimes slightly differ from regular one (no `.tapBoth`, different `.tap`)
  val zioLike_notStream = zioTypes ++ managedTypes ++ extraTypes ++ zioTestAsserts
  val zioLikePackages   = zioLike_notStream ++ zioStreamTypes

  def fromZioLike(r: ScExpression): Boolean =
    isOfClassFrom(r, zioLikePackages)

  def fromZioTestAsserts(r: ScExpression): Boolean =
    isOfClassFrom(r, zioTestAsserts)

  def fromZioLike(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioLikePackages)

  def fromZio(r: ScExpression): Boolean =
    isOfClassFrom(r, zioTypes)

  def fromZio(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioTypes)

  def fromManaged(tpe: ScType): Boolean =
    isOfClassFrom(tpe, managedTypes)

  def fromZioSink(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioSinkTypes)

  def fromZioStream(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioStreamTypes)

  def fromZioLayer(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioLayerTypes)

  def fromZioSpec(r: ScExpression): Boolean =
    isOfClassFrom(r, zioSpecTypes)

  def fromZioSpec(tpe: ScType): Boolean =
    isOfClassFrom(tpe, zioSpecTypes)

  sealed trait TypeArgs2Extractor {

    protected def fromTarget(tpe: ScType): Boolean

    private def unapplyInner(tpe: ScType): Option[(ScType, ScType)] =
      extractAllTypeArguments(tpe).flatMap {
        case Seq(r, e) => Some(r, e)
        case _         => None
      }

    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      if (fromTarget(tpe)) unapplyInner(tpe) else None

  }

  sealed trait TypeArgs3Extractor {

    protected def fromTarget(tpe: ScType): Boolean

    private def unapplyInner(tpe: ScType): Option[(ScType, ScType, ScType)] =
      extractAllTypeArguments(tpe).flatMap {
        case Seq(r, e, a) => Some(r, e, a)
        case _            => None
      }

    def unapply(tpe: ScType): Option[(ScType, ScType, ScType)] =
      if (fromTarget(tpe)) unapplyInner(tpe) else None

  }

  object `ZIO[R, E, A]` extends TypeArgs3Extractor {
    override protected def fromTarget(tpe: ScType): Boolean = fromZio(tpe)
  }

  object `URIO[R, A]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZIO[R, E, A]`(r, e, a) if e.isNothing => Some(r, a)
        case _                                      => None
      }
  }

  object `IO[E, A]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZIO[R, E, A]`(r, e, a) if r.isAny => Some(e, a)
        case _                                  => None
      }
  }

  object `ZManaged[R, E, A]` extends TypeArgs3Extractor {
    override protected def fromTarget(tpe: ScType): Boolean = fromManaged(tpe)
  }

  object `URManaged[R, A]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZManaged[R, E, A]`(r, e, a) if e.isNothing => Some(r, a)
        case _                                           => None
      }
  }

  object `Managed[E, A]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZManaged[R, E, A]`(r, e, a) if r.isAny => Some(e, a)
        case _                                       => None
      }
  }

  object `ZStream[R, E, O]` extends TypeArgs3Extractor {
    override protected def fromTarget(tpe: ScType): Boolean = fromZioStream(tpe)
  }

  object `URStream[R, O]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZStream[R, E, O]`(r, e, a) if e.isNothing => Some(r, a)
        case _                                          => None
      }
  }

  object `Stream[E, O]` {
    def unapply(tpe: ScType): Option[(ScType, ScType)] =
      tpe match {
        case `ZStream[R, E, O]`(r, e, a) if r.isAny => Some(e, a)
        case _                                      => None
      }
  }

  object `ZLayer[RIn, E, ROut]` extends TypeArgs3Extractor {
    override protected def fromTarget(tpe: ScType): Boolean = fromZioLayer(tpe)
  }

  object `zio1.Spec[R, E, T]` extends TypeArgs3Extractor {
    override protected def fromTarget(tpe: ScType): Boolean = fromZioSpec(tpe)
  }

  object `zio2.Spec[R, E]` extends TypeArgs2Extractor {
    override protected def fromTarget(tpe: ScType): Boolean = fromZioSpec(tpe)
  }

  def isInfallibleEffect(tpe: ScType): Boolean =
    tpe match {
      case `URIO[R, A]`(_, _)      => true
      case `URManaged[R, A]`(_, _) => true
      case `URStream[R, O]`(_, _)  => true
      case _                       => false
    }

  def isInfallibleEffect(tpe: TypeResult): Boolean =
    isInfallibleEffect(tpe.getOrAny)

  def isInfallibleEffect(zio: ScExpression): Boolean =
    isInfallibleEffect(zio.`type`())

  def isAnyEnvEffect(zio: ScExpression): Boolean =
    zio.`type`().getOrAny match {
      case `IO[E, A]`(_, _)      => true
      case `Managed[E, A]`(_, _) => true
      case `Stream[E, O]`(_, _)  => true
      case _                     => false
    }

}
