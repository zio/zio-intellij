package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.simplifications.SimplifyCollectAllInspection

abstract class CollectAllInspectionTest(methodToReplace: String, methodToReplaceWith: String, isParN: Boolean = false)
    extends ZSimplifyInspectionTest[SimplifyCollectAllInspection] {
  import EditorTestUtil.{ SELECTION_END_TAG => END, SELECTION_START_TAG => START }

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
         |ZIO.$methodToReplaceWith$nParamList(myIterable)(f)""".stripMargin
    )
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
