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
        "dev.zio" %% "zio" % "1.0.0-RC18-2",
        "dev.zio" %% "zio-test" % "1.0.0-RC18-2"
      )

  def z(s: String): String =
    s"""import zio._
       |import zio.console._
       |import zio.test._
       |import zio.test.Assertion._
       |object Test {
       |  def foo = {
       |   $s
       | }
       |}
       |""".stripMargin

  implicit protected class S(s: String) {
    def assertHighlighted(): Unit = checkTextHasError(s)
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
