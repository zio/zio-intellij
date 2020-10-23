package zio.intellij.utils

import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import org.jetbrains.plugins.scala.lang.psi.types.result.TypeResult
import zio.intellij.utils.types.{ZLayerTypes, ZioTypes}

object TypeCheckUtils {

  val zioTypes        = ZioTypes.values.map(_.fqName).toArray
  val zioLayerTypes   = ZLayerTypes.values.map(_.fqName).toArray
  val zioSinkTypes    = Array("zio.stream.ZSink")
  val zioStreamTypes  = Array("zio.stream.ZStream")
  val managedTypes    = Array("zio.ZManaged")
  val extraTypes      = Array("zio.Fiber", "zio.ZQueue", "zio.ZRef", "zio.ZRefM", "zio.ZQuery")
  val zioTest         = Array("zio.test._")
  val zioLikePackages = zioTypes ++ zioStreamTypes ++ managedTypes ++ extraTypes ++ zioTest

  def fromZioLike(r: ScExpression): Boolean =
    isOfClassFrom(r, zioLikePackages)

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

  sealed trait TypeArgs3Extractor {

    protected def fromTarget(tpe: ScType): Boolean

    private def unapplyInner(tpe: ScType): Option[(ScType, ScType, ScType)] =
      resolveAliases(tpe.tryExtractDesignatorSingleton).flatMap(extractTypeArguments).flatMap {
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
    isInfallibleEffect(zio.`type`)

}
