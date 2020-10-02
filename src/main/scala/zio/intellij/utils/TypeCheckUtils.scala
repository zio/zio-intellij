package zio.intellij.utils

import org.jetbrains.plugins.scala.codeInspection.collections.isOfClassFrom
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.types.ScType
import zio.intellij.utils.types.ZioTypes

object TypeCheckUtils {

  val zioTypes        = ZioTypes.values.map(_.fqName).toArray
  val managedTypes    = Array("zio.ZManaged")
  val extraTypes      = Array("zio.Fiber", "zio.ZQueue", "zio.ZRef", "zio.ZRefM", "zio.ZQuery")
  val zioTest         = Array("zio.test._")
  val zioLikePackages = zioTypes ++ managedTypes ++ extraTypes ++ zioTest

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

}
