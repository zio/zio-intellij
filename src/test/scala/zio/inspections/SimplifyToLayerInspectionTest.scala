package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyToLayerInspection

sealed abstract class BaseToLayerInspectionTest(methodToReplace: String, methodToReplaceWith: String)
    extends ZSimplifyInspectionTest[SimplifyToLayerInspection] {
  override protected val hint = s"Replace with .$methodToReplaceWith"

  private def base(expr: String) =
    s"""trait Service
       |val serviceEffect = ZIO.succeed(new Service {})
       |$expr""".stripMargin

  private val zLayerExpr = s"ZLayer.$methodToReplace(serviceEffect)"

  def testHighlighting(): Unit =
    z(base(s"$START$zLayerExpr$END")).assertHighlighted()

  def testReplacement(): Unit = {
    val text   = z(base(zLayerExpr))
    val result = z(base(s"serviceEffect.$methodToReplaceWith"))
    testQuickFix(text, result, hint)
  }
}

class SimplifyToLayerInspectionTest
    extends BaseToLayerInspectionTest(methodToReplace = "fromEffect", methodToReplaceWith = "toLayer")

class SimplifyToLayerManyInspectionTest
    extends BaseToLayerInspectionTest(methodToReplace = "fromEffectMany", methodToReplaceWith = "toLayerMany")
