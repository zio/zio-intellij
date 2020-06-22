package zio.intellij

import com.intellij.psi.search.searches.ReferencesSearch
import org.jetbrains.plugins.scala.annotator.usageTracker.ScalaRefCountHolder
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement

package object utils {

  // taken from ScalaUnusedSymbolInspection
  def isElementUsed(element: ScNamedElement, isOnTheFly: Boolean): Boolean =
    if (isOnTheFly) {
      //we can trust RefCounter because references are counted during highlighting
      val refCounter = ScalaRefCountHolder(element)
      var used       = false

      val success = refCounter.retrieveUnusedReferencesInfo { () =>
        used |= refCounter.isValueReadUsed(element) || refCounter.isValueWriteUsed(element)
      }

      !success || used //want to return true if it was a failure
    } else {
      //need to look for references because file is not highlighted
      ReferencesSearch.search(element, element.getUseScope).findFirst() != null
    }

  // CompositeOrdering is taken from https://stackoverflow.com/a/14696410
  final class CompositeOrdering[T](val ord1: Ordering[T], val ord2: Ordering[T]) extends Ordering[T] {

    def compare(x: T, y: T): Int = {
      val comp = ord1.compare(x, y)
      if (comp != 0) comp else ord2.compare(x, y)
    }
  }

  object CompositeOrdering {
    def apply[T](orderings: Ordering[T]*): Ordering[T] = orderings.reduceLeft(_.orElse(_))
  }

  implicit final class OrderingOps[T](private val ord: Ordering[T]) extends AnyVal {
    def orElse(ord2: Ordering[T]) = new CompositeOrdering[T](ord, ord2)
  }

  def trimAfterSuffix(str: String, suffix: String): String = {
    val idx = str.lastIndexOf(suffix)
    if (idx < 0) str
    else str.substring(0, idx + suffix.length)
  }

}
