package zio.intellij.debugger

import com.intellij.ui.classFilter.{ClassFilter, DebuggerClassFilterProvider}
import zio.intellij.debugger.ZioSteppingFilterProvider.filterPatterns

import java.util
import scala.jdk.CollectionConverters._

/**
 *  Prevents stepping into classes defined in the ZIO package
 */
final class ZioSteppingFilterProvider extends DebuggerClassFilterProvider {
  override def getFilters: util.List[ClassFilter] = filterPatterns
}
object ZioSteppingFilterProvider {
  val filterPatterns = List("zio.*").map(new ClassFilter(_)).asJava
}
