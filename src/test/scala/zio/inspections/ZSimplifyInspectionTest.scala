package zio.inspections

import com.intellij.codeInspection.LocalInspectionTool
import intellij.testfixtures._
import org.jetbrains.plugins.scala.base.libraryLoaders._
import org.jetbrains.plugins.scala.codeInspection.ScalaInspectionTestBase
import org.jetbrains.plugins.scala.codeInspection.collections._
import zio.intellij.inspections.ZInspection

import scala.reflect._

trait ZInspectionTestBase[T <: LocalInspectionTool] { base: ScalaInspectionTestBase =>

  override protected def librariesLoaders: Seq[LibraryLoader] =
    base.librariesLoaders :+
      IvyManagedLoader(
        "dev.zio" %% "zio"         % "latest.integration",
        "dev.zio" %% "zio-streams" % "latest.integration",
        "dev.zio" %% "zio-test"    % "latest.integration"
      )

  def z(s: String): String =
    s"""import zio._
       |import zio.console._
       |import zio.duration._
       |import zio.stream._
       |import zio.test._
       |import zio.test.Assertion._
       |import scala.concurrent.Future
       |import scala.concurrent.ExecutionContext.Implicits.global
       |import scala.util._
       |object Test {
       |
       |  val logger: Logger = null
       |  val b = ZIO.unit
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

  implicit protected class S(s: String) {
    def assertHighlighted(): Unit = checkTextHasError(s)

    def assertNotHighlighted(): Unit = {
      var thrownEx: AssertionError = null // ;(
      try checkTextHasError(s)
      catch {
        case e: AssertionError => thrownEx = e
      } finally if (thrownEx != null) ()
      else throw new AssertionError("An error from the highlighter was expected to be thrown, but wasn't.")
    }

  }
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
