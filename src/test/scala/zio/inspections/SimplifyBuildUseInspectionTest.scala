package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyBuildUseInspection
import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}

class SimplifyBuildUseInspectionTest extends ZSimplifyInspectionTest[SimplifyBuildUseInspection] {
  override protected val hint: String = "Replace with .provideLayer"

  private def zz(s: String): String =
    z {
      s"""
         |val has: Has[Int] = Has(1)
         |val layer: ULayer[Has[Int]] = ZLayer.succeedMany(has)
         |val effect: ZIO[Has[Int], Nothing, String] = ZIO.service[Int].map(_.toString)
         |$s
         |""".stripMargin
    }

  def testRefHighlighting(): Unit =
    zz(s"${START}layer.build.use(effect.provide)$END").assertHighlighted()

  def testRefReplacement(): Unit = {
    val text   = zz("layer.build.use(effect.provide)")
    val result = zz("effect.provideLayer(layer)")
    testQuickFixes(text, result, hint)
  }

  def testUnderscoreHighlighting(): Unit =
    zz(s"${START}layer.build.use(effect.provide(_))$END").assertHighlighted()

  def testUnderscoreReplacement(): Unit = {
    val text   = zz("layer.build.use(effect.provide(_))")
    val result = zz("effect.provideLayer(layer)")
    testQuickFixes(text, result, hint)
  }

  def testLambdaHighlighting(): Unit =
    zz(s"${START}layer.build.use(l => effect.provide(l))$END").assertHighlighted()

  def testLambdaReplacement(): Unit = {
    val text   = zz("layer.build.use(l => effect.provide(l))")
    val result = zz("effect.provideLayer(layer)")
    testQuickFixes(text, result, hint)
  }

  def testLambdaNoHighlighting(): Unit =
    zz(s"${START}layer.build.use(l => effect.provide(has))$END").assertNotHighlighted()
}
