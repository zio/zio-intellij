package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyCollectAllInspection

abstract class CollectAllInspectionTest(methodToReplace: String, methodToReplaceWith: String, isParN: Boolean = false)
    extends ZSimplifyInspectionTest[SimplifyCollectAllInspection] {

  private val nParamList = if (isParN) "(n)" else ""

  override protected val hint: String = s"Replace with ZIO.$methodToReplaceWith"

  def testHighlighting(): Unit =
    z(s"""val myIterable: Iterable[String] = ???
         |${START}URIO.$methodToReplace$nParamList(myIterable.map(f))$END""".stripMargin).assertHighlighted()

  def testReplacement(): Unit = {
    val text = z(
      s"""val myIterable: Iterable[String] = ???
         |URIO.$methodToReplace$nParamList(myIterable.map(f))""".stripMargin
    )
    val result = z(
      s"""val myIterable: Iterable[String] = ???
         |URIO.$methodToReplaceWith$nParamList(myIterable)(f)""".stripMargin
    )
    testQuickFix(text, result, hint)
  }

  def testBlockHighlighting(): Unit =
    z {
      s"""val myIterable: Iterable[String] = ???
         |${START}URIO.$methodToReplace$nParamList(myIterable.map { it =>
         |  b
         |  b
         |  b
         |  f(it)
         |})$END""".stripMargin
    }.assertHighlighted()

  def testBlockReplacement(): Unit = {
    val text = z {
      s"""val myIterable: Iterable[String] = ???
         |URIO.$methodToReplace$nParamList(myIterable.map { it =>
         |  b
         |  b
         |  b
         |  f(it)
         |})""".stripMargin
    }
    val result = z {
      s"""val myIterable: Iterable[String] = ???
         |URIO.$methodToReplaceWith$nParamList(myIterable) {
         |  it =>
         |    b
         |    b
         |    b
         |    f(it)
         |}""".stripMargin
    }
    testQuickFix(text, result, hint)
  }
}

class SimplifyCollectAllToForeachTest
    extends CollectAllInspectionTest(methodToReplace = "collectAll", methodToReplaceWith = "foreach")

class SimplifyCollectAllParToForeachParTest
    extends CollectAllInspectionTest(methodToReplace = "collectAllPar", methodToReplaceWith = "foreachPar")

class SimplifyCollectAllParNToForeachParNTest
    extends CollectAllInspectionTest(
      methodToReplace = "collectAllParN",
      methodToReplaceWith = "foreachParN",
      isParN = true
    )
