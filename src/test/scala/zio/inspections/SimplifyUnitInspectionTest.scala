package zio.inspections

import com.intellij.testFramework.EditorTestUtil
import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG, SELECTION_START_TAG}
import org.jetbrains.plugins.scala.codeInspection.collections._
import zio.intellij.inspections.simplifications.SimplifyUnitInspection

class SimplifyUnitInspectionTest extends OperationsOnCollectionInspectionTest {
  import EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

  override protected val classOfInspection: Class[_ <: OperationOnCollectionInspection] =
    classOf[SimplifyUnitInspection]

  override protected val hint = "Replace with .unit"

  def test_1() {
    val selected = s"ZIO.succeed(42) $START*> ZIO.unit$END"
    checkTextHasError(selected)
    val text = "ZIO.succeed(42) *> ZIO.unit"
    val result = "ZIO.succeed(42).unit"
    testQuickFix(text, result, hint)
  }
}
