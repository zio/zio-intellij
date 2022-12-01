package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyForeachInspection

abstract class SimplifyForeachInspectionTest(
  methodToReplace: String,
  methodToReplaceWith: String,
  isParN: Boolean = false
) extends ZSimplifyInspectionTest[SimplifyForeachInspection] {

  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  private val nParamList = if (isParN) "(n)" else ""

  private val zioMethodToReplace     = s"""URIO.$methodToReplace$nParamList(myIterable)(f)"""
  private val zioMethodToReplaceWith = s"""URIO.$methodToReplaceWith$nParamList(myIterable)(f)"""

  private val zioBlockMethodToReplace =
    s"""URIO.$methodToReplace$nParamList(myIterable) { it =>
       |  println(it)
       |  for {
       |    _ <- ZIO.fail(???)
       |  } yield ()
       |}""".stripMargin

  private val zioBlockMethodToReplaceWith =
    s"""URIO.$methodToReplaceWith$nParamList(myIterable) {
       |  it =>
       |    println(it)
       |    for {
       |      _ <- ZIO.fail(???)
       |    } yield ()
       |}""".stripMargin

  override protected val hint: String = s"Replace with ZIO.$methodToReplaceWith"

  def testForCompHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- $START$zioMethodToReplace$END
         |} yield ???""".stripMargin
    }.assertHighlighted()

  def testForCompReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- $zioMethodToReplace
         |} yield ???""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- $zioMethodToReplaceWith
         |} yield ???""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testForCompBlockHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- $START$zioBlockMethodToReplace$END
         |} yield ???""".stripMargin
    }.assertHighlighted()

  def testForCompBlockReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- $zioBlockMethodToReplace
         |} yield ???""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- $zioBlockMethodToReplaceWith
         |} yield ???""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testNestedForCompHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |  _ <- $START$zioMethodToReplace$END
         |  _ <- b
         |} yield ???""".stripMargin
    }.assertHighlighted()

  def testNestedForCompReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |  _ <- $zioMethodToReplace
         |  _ <- b
         |} yield ???""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |} yield ???""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testNestedBlockForCompHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |  _ <- $START$zioBlockMethodToReplace$END
         |  _ <- b
         |} yield ???""".stripMargin
    }.assertHighlighted()

  def testNestedBlockForCompReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |  _ <- $zioBlockMethodToReplace
         |  _ <- b
         |} yield ???""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |for {
         |  _ <- b
         |  _ <- $zioMethodToReplaceWith
         |  _ <- b
         |  _ <- $zioBlockMethodToReplaceWith
         |  _ <- b
         |} yield ???""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testChainHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |$START$zioMethodToReplace$END *> b""".stripMargin
    }.assertHighlighted()

  def testChainReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |$zioMethodToReplace *> b""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |$zioMethodToReplaceWith *> b""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testBlockChainHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |$START$zioBlockMethodToReplace$END *> b""".stripMargin
    }.assertHighlighted()

  def testBlockChainReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |$zioBlockMethodToReplace *> b""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |$zioBlockMethodToReplaceWith *> b""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testNestedChainHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |b <* b *> b *> $START$zioMethodToReplace$END *> b""".stripMargin
    }.assertHighlighted()

  def testNestedChainReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |b <* b *> b *> $zioMethodToReplace *> b""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |b <* b *> b *> $zioMethodToReplaceWith *> b""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testNestedBlockChainHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |b <* b *> b *> $START$zioBlockMethodToReplace$END *> b""".stripMargin
    }.assertHighlighted()

  def testNestedBlockChainReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |b <* b *> b *> $zioBlockMethodToReplace *> b""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |b <* b *> b *> $zioBlockMethodToReplaceWith *> b""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

}

class SimplifyForeachToForeach_Test
    extends SimplifyForeachInspectionTest(methodToReplace = "foreach", methodToReplaceWith = "foreach_")

class SimplifyForeachParToForeachPar_Test
    extends SimplifyForeachInspectionTest(methodToReplace = "foreachPar", methodToReplaceWith = "foreachPar_")

class SimplifyForeachParNToForeachParN_Test
    extends SimplifyForeachInspectionTest(
      methodToReplace = "foreachParN",
      methodToReplaceWith = "foreachParN_",
      isParN = true
    )
