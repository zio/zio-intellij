package zio.inspections

import com.intellij.codeInspection.LocalInspectionTool
import intellij.testfixtures._
import org.jetbrains.plugins.scala.base.libraryLoaders._
import org.jetbrains.plugins.scala.codeInspection.ScalaInspectionTestBase
import org.jetbrains.plugins.scala.codeInspection.collections._
import zio.inspections.ZInspectionTestBase._
import zio.intellij.inspections.ZInspection

import scala.reflect._

trait ZInspectionTestBase[T <: LocalInspectionTool] { base: ScalaInspectionTestBase =>

  protected def isZIO1 = true

  override protected def librariesLoaders: Seq[LibraryLoader] =
    Seq(
      ScalaSDKLoader(),
      IvyManagedLoader(
        "dev.zio" %% "zio"         % versionPattern,
        "dev.zio" %% "zio-streams" % versionPattern,
        "dev.zio" %% "zio-test"    % versionPattern
      )
    )

  def z(s: String): String =
    s"""${`import zio._`}
       |import scala.concurrent.Future
       |import scala.concurrent.ExecutionContext.Implicits.global
       |import scala.util._
       |object Test {
       |
       |  val logger: Logger = null
       |  val a: String = "hello"
       |  val b = ZIO.unit
       |  val o: Option[Int] = Some(1)
       |  def f(a: Any): ZIO[Any, Throwable, Unit] = ???
       |  def f(a: Any, b: Any): ZIO[Any, Throwable, Unit] = ???
       |
       |  def foo = {
       |   $s
       | }
       |}
       |trait Logger {
       |  def log[A](a: A): ZIO[Any, Nothing, Unit]
       |}
       |""".stripMargin

  def Has(tpe: String): String = if (isZIO1) s"Has[$tpe]" else tpe

  def range(str: String): String = s"$START$str$END"

  @inline def r(str: String): String = range(str)

  implicit protected class S(s: String) {
    def assertHighlighted(): Unit = checkTextHasError(s)

    def assertNotHighlighted(): Unit = {
      var thrownEx: AssertionError = null // ;(
      try checkTextHasError(s)
      catch {
        case e: AssertionError if e.getMessage.contains("not found:") => thrownEx = e
      } finally if (thrownEx != null) ()
      else throw new AssertionError("An error from the highlighter was expected to be thrown, but wasn't.")
    }
  }

  private val versionPattern = if (isZIO1) versionPatternZIO1 else versionPatternZIO2
  private val `import zio._` =
    if (isZIO1) {
      """import zio._
        |import zio.console._
        |import zio.duration._
        |import zio.stream._
        |import zio.test._
        |import zio.test.Assertion._""".stripMargin
    } else {
      """import zio._
        |import zio.stream._
        |import zio.test._
        |import zio.test.Assertion._""".stripMargin
    }

}

object ZInspectionTestBase {
  // https://ant.apache.org/ivy/history/2.3.0/ivyfile/dependency.html#revision
  val versionPatternZIO1 = "(,2.0[" // matches all versions lower than 2.0
  val versionPatternZIO2 = "[2.0,)" // matches all versions greater or equal to 2.0
}

abstract class ZSimplifyInspectionTest[T <: ZInspection: ClassTag]
    extends OperationsOnCollectionInspectionTest
    with ZInspectionTestBase[T] {

  final override protected val classOfInspection =
    classTag[T].runtimeClass.asInstanceOf[Class[_ <: ZInspection]]
}

abstract class ZScalaInspectionTest[T <: LocalInspectionTool: ClassTag]
    extends ScalaInspectionTestBase
    with ZInspectionTestBase[T] {

  final override protected val classOfInspection =
    classTag[T].runtimeClass.asInstanceOf[Class[_ <: LocalInspectionTool]]
}
