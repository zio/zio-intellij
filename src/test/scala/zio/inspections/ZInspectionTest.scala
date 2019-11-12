package zio.inspections

import intellij.testfixtures._
import org.jetbrains.plugins.scala.base.libraryLoaders._
import org.jetbrains.plugins.scala.codeInspection.collections._
import zio.intellij.inspections.ZInspection

import scala.reflect._

abstract class ZInspectionTest[T <: ZInspection: ClassTag] extends OperationsOnCollectionInspectionTest {
  final override protected val classOfInspection = classTag[T].runtimeClass.asInstanceOf[Class[_ <: ZInspection]]

  override protected def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader(
        "dev.zio" %% "zio" % "1.0.0-RC16"
      )

  def z(s: String): String =
    s"""import zio._
       |import scala._
       |object Test {
       |  def foo = {
       |   $s
       | }
       |}
       |""".stripMargin

  protected implicit class S(s: String) {
    def assertHighlighted(): Unit = checkTextHasError(s)
  }
}
