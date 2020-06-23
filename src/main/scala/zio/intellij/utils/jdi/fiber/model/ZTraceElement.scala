package zio.intellij.utils.jdi.fiber.model

/**
 * This class was copied from the zio project
 *
 * https://github.com/zio/zio/blob/e9124af60becf2cab93aa92cdab392975fb94664/stacktracer/shared/src/main/scala/zio/internal/stacktracer/ZTraceElement.scala#L19
 */
sealed abstract class ZTraceElement extends Product with Serializable {
  def prettyPrint: String
}

object ZTraceElement {

  final case class NoLocation(error: String) extends ZTraceElement {
    def prettyPrint = s"<couldn't get location, error: $error>"
  }

  final case class SourceLocation(file: String, clazz: String, method: String, line: Int) extends ZTraceElement {
    def toStackTraceElement: StackTraceElement = new StackTraceElement(clazz, method, file, line)
    def prettyPrint: String                    = toStackTraceElement.toString
  }
}
