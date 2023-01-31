package zio.inspections

import zio.intellij.inspections.ZInspection
import zio.intellij.inspections.simplifications.SimplifyForeachInspectionZIO1
import zio.intellij.inspections.simplifications.SimplifyForeachInspectionZIO2

import scala.reflect.ClassTag

abstract class SimplifyForeachInspectionTest[S <: ZInspection: ClassTag](
  methodToReplace: String,
  methodToReplaceWith: String,
  isParN: Boolean = false
) extends ZSimplifyInspectionTest[S] {

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

class SimplifyForeachToForeach_ZIO1Test
    extends SimplifyForeachInspectionTest[SimplifyForeachInspectionZIO1](
      methodToReplace = "foreach",
      methodToReplaceWith = "foreach_"
    )

class SimplifyForeachToForeach_ZIO2Test
    extends SimplifyForeachInspectionTest[SimplifyForeachInspectionZIO2](
      methodToReplace = "foreach",
      methodToReplaceWith = "foreachDiscard"
    )

class SimplifyForeachParToForeachPar_ZIO1Test
    extends SimplifyForeachInspectionTest[SimplifyForeachInspectionZIO1](
      methodToReplace = "foreachPar",
      methodToReplaceWith = "foreachPar_"
    )

class SimplifyForeachParToForeachPar_ZIO2Test
    extends SimplifyForeachInspectionTest[SimplifyForeachInspectionZIO2](
      methodToReplace = "foreachPar",
      methodToReplaceWith = "foreachParDiscard"
    )

class SimplifyForeachParNToForeachParN_Test
    extends SimplifyForeachInspectionTest[SimplifyForeachInspectionZIO1](
      methodToReplace = "foreachParN",
      methodToReplaceWith = "foreachParN_",
      isParN = true
    )
