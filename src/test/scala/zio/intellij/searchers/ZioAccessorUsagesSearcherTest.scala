package zio.intellij.searchers

import com.intellij.openapi.util.text.StringUtil
import com.intellij.psi.util.PsiTreeUtil
import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScNamedElement
import org.jetbrains.plugins.scala.util.Markers

import scala.jdk.CollectionConverters._

class ZioAccessorUsagesSearcherTest extends ScalaLightCodeInsightFixtureTestAdapter with Markers {

  protected val zioOrg     = "dev.zio"
  protected val zioVersion = "latest.integration"

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+ IvyManagedLoader(zioOrg %% "zio" % zioVersion, zioOrg %% "zio-macros" % zioVersion)

  private def doTest(fileText: String): Unit = {

    val (source, expectedUsageRanges) = extractMarkers(StringUtil.convertLineSeparators(fileText))

    configureFromFileText(source)

    val elem  = myFixture.getElementAtCaret
    val named = PsiTreeUtil.getParentOfType(elem, classOf[ScNamedElement], false)

    val foundUsages = myFixture.findUsages(named).asScala.map(_.getElement.getTextRange).toSet

    val expectedButNotFound = expectedUsageRanges.filterNot(foundUsages.contains)
    val foundButNotExpected = foundUsages.filterNot(expectedUsageRanges.contains)

    assert(
      expectedButNotFound.isEmpty,
      s"Didn't find ${expectedButNotFound.mkString(", ")} but found ${foundButNotExpected.mkString(", ")}"
    )
    assert(foundButNotExpected.isEmpty, s"Found but didn't expect ${foundButNotExpected.mkString(", ")}")
  }

  def base(s: String): String =
    s"""import zio._
       |import zio.macros.accessible
       |
       |
       |$s
       |""".stripMargin

  // Two simple checks for zio macros
  // It should work fine if regular `find usages` works fine
  def testMacrosVal(): Unit =
    doTest(base(s"""@accessible
                   |object FindMyAccessors {
                   |  trait Service {
                   |    val method$caretText: UIO[Int] = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testMacrosDef(): Unit =
    doTest(base(s"""@accessible
                   |object FindMyAccessors {
                   |  trait Service {
                   |    def method$caretText: UIO[Int] = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindValZioValZio(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    val method$caretText: UIO[Int] = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindDefZioDefZio(): Unit =
    doTest(base(s"""
                   |import zio._
                   |object FindMyAccessors {
                   |  def method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText: UIO[Int] = ???
                   |  }
                   |}
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindValZioVal(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    val method$caretText: Int = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindDefZioDef(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText: Int = ???
                   |  }
                   |}
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindValValZio(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: Int = ???
                   |  trait Service {
                   |    val method$caretText: UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindDefDefZio(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method: Int = ???
                   |  trait Service {
                   |    def method$caretText: UIO[Int] = ???
                   |  }
                   |}
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindValVal(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: Int = ???
                   |  trait Service {
                   |    val method$caretText: Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindDefDef(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method: Int = ???
                   |  trait Service {
                   |    def method$caretText: Int = ???
                   |  }
                   |}
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindDefZioDefZioOneArgSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("")
      """.stripMargin))

  def testFindDefZioDefZioOneArgDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefZioDefZioTwoArgsSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("", 1)
      """.stripMargin))

  def testFindDefZioDefZioTwoArgsDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindDefZioDefOneArgSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): Int = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("")
      """.stripMargin))

  def testFindDefZioDefOneArgDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefZioDefTwoArgsSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int = ???
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("", 1)
      """.stripMargin))

  def testFindDefZioDefTwoArgsDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindDefDefZioOneArgSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefZioOneArgDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefZioTwoArgsSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("", 1)
      """.stripMargin))

  def testFindDefDefZioTwoArgsDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int] = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindDefDefOneArgSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefOneArgDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefTwoArgsSame(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("", 1)
      """.stripMargin))

  def testFindDefDefTwoArgsDiff(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int = ???
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindValZioValZioAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    val method$caretText: UIO[Int]
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindDefZioDefZioAbstract(): Unit =
    doTest(base(s"""
                   |import zio._
                   |object FindMyAccessors {
                   |  def method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText: UIO[Int]
                   |  }
                   |}
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindValZioValAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    val method$caretText: Int
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindDefZioDefAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method: URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText: Int
                   |  }
                   |}
                   |${start}FindMyAccessors.method$end
      """.stripMargin))

  def testFindValValZioAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: Int = ???
                   |  trait Service {
                   |    val method$caretText: UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindDefDefZioAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method: Int = ???
                   |  trait Service {
                   |    def method$caretText: UIO[Int]
                   |  }
                   |}
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindValValAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  val method: Int = ???
                   |  trait Service {
                   |    val method$caretText: Int
                   |  }
                   |}
                   |
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindDefDefAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method: Int = ???
                   |  trait Service {
                   |    def method$caretText: Int
                   |  }
                   |}
                   |FindMyAccessors.method
      """.stripMargin))

  def testFindDefZioDefZioOneArgSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): UIO[Int]
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("")
      """.stripMargin))

  def testFindDefZioDefZioOneArgDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefZioDefZioTwoArgsSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int]
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("", 1)
      """.stripMargin))

  def testFindDefZioDefZioTwoArgsDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindDefZioDefOneArgSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): Int
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("")
      """.stripMargin))

  def testFindDefZioDefOneArgDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): Int
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefZioDefTwoArgsSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int
                   |  }
                   |}
                   |
                   |${start}FindMyAccessors.method$end("", 1)
      """.stripMargin))

  def testFindDefZioDefTwoArgsDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): URIO[Has[Service], Int] = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindDefDefZioOneArgSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefZioOneArgDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefZioTwoArgsSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("", 1)
      """.stripMargin))

  def testFindDefDefZioTwoArgsDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): UIO[Int]
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

  def testFindDefDefOneArgSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String): Int
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefOneArgDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: Int): Int
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("")
      """.stripMargin))

  def testFindDefDefTwoArgsSameAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: String, arg2: Int): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int): Int
                   |  }
                   |}
                   |
                   |FindMyAccessors.method("", 1)
      """.stripMargin))

  def testFindDefDefTwoArgsDiffAbstract(): Unit =
    doTest(base(s"""
                   |object FindMyAccessors {
                   |  def method(arg1: Int, arg2: String): Int = ???
                   |  trait Service {
                   |    def method$caretText(arg1: String, arg2: Int)
                   |  }
                   |}
                   |
                   |FindMyAccessors.method(1, "")
      """.stripMargin))

}
