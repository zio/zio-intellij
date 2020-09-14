package zio.intellij.utils

// Taken from Scala 2.13 code base
// https://github.com/scala/scala/blob/26dd17aebc988ba84c243e6f23796680df0d4a26/src/library/scala/Option.scala#L40
object OptionUtils {

  /**
   * When a given condition is true, evaluates the `a` argument and returns
   *  Some(a). When the condition is false, `a` is not evaluated and None is
   *  returned.
   */
  def when[A](cond: Boolean)(a: => A): Option[A] =
    if (cond) Some(a) else None

  /**
   * Unless a given condition is true, this will evaluate the `a` argument and
   *  return Some(a). Otherwise, `a` is not evaluated and None is returned.
   */
  @inline def unless[A](cond: Boolean)(a: => A): Option[A] =
    when(!cond)(a)

}
