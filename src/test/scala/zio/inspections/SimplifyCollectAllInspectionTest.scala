package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import zio.intellij.inspections.ZInspection
import zio.intellij.inspections.simplifications.{
  SimplifyCollectAllInspection,
  SimplifyCollectAllToForeachInspection,
  SimplifyCollectAllToTraverseInspection
}

import scala.reflect.ClassTag

abstract class CollectAllInspectionTest[I <: SimplifyCollectAllInspection with ZInspection: ClassTag](
  methodToReplace: String,
  methodToReplaceWith: String
) extends ZInspectionTest[I] {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val hint: String = s"Replace with ZIO.$methodToReplaceWith"

  def testHighlighting(): Unit =
    z(s"""val myIterable: Iterable[String] = ???
         |${START}URIO.$methodToReplace(myIterable.map(f))$END""".stripMargin).assertHighlighted()

  def testReplacement(): Unit = {
    val text = z(
      s"""val myIterable: Iterable[String] = ???
         |URIO.$methodToReplace(myIterable.map(f))""".stripMargin
    )
    val result = z(
      s"""val myIterable: Iterable[String] = ???
         |ZIO.$methodToReplaceWith(myIterable)(f)""".stripMargin
    )
    testQuickFix(text, result, hint)
  }
}

class SimplifyCollectAllToForeachTest
    extends CollectAllInspectionTest[SimplifyCollectAllToForeachInspection](
      methodToReplace = "collectAll",
      methodToReplaceWith = "foreach"
    )

class SimplifyCollectAllParToForeachParTest
    extends CollectAllInspectionTest[SimplifyCollectAllToForeachInspection](
      methodToReplace = "collectAllPar",
      methodToReplaceWith = "foreachPar"
    )

class SimplifyCollectAllToTraverseTest
    extends CollectAllInspectionTest[SimplifyCollectAllToTraverseInspection](
      methodToReplace = "collectAll",
      methodToReplaceWith = "traverse"
    )

class SimplifyCollectAllParToTraverseParTest
    extends CollectAllInspectionTest[SimplifyCollectAllToTraverseInspection](
      methodToReplace = "collectAllPar",
      methodToReplaceWith = "traversePar"
    )
