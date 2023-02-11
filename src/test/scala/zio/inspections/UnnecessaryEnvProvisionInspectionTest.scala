package zio.inspections

import zio.intellij.inspections.mistakes.UnnecessaryEnvProvisionInspection

abstract class UnnecessaryEnvProvisionInspectionTest(fromEffect: String => String)
    extends ZScalaInspectionTest[UnnecessaryEnvProvisionInspection] {
  private val faultyMethod = "provideSomeLayer"

  override protected val description = "Effect doesn't require environment; there is no need to provide it"
  private val hint                   = UnnecessaryEnvProvisionInspection.hint(faultyMethod)

  def testInlineHighlighting(): Unit =
    z(s"$START${fromEffect("UIO(1)")}.$faultyMethod(???)$END").assertHighlighted()

  def testInlineReplacement(): Unit = {
    val text   = z(s"${fromEffect("UIO(1)")}.$faultyMethod(???)")
    val result = z(fromEffect("UIO(1)"))
    testQuickFix(text, result, hint)
  }

  def testValHighlighting(): Unit = z {
    s"""val foo = ${fromEffect("UIO(1)")}
       |${START}foo.$faultyMethod(???)$END""".stripMargin
  }.assertHighlighted()

  def testValReplacement(): Unit = {
    val text = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod(???)""".stripMargin
    }
    val result = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testDefHighlighting(): Unit = z {
    s"""def foo = ${fromEffect("UIO(1)")}
       |${START}foo.$faultyMethod(???)$END""".stripMargin
  }.assertHighlighted()

  def testDefReplacement(): Unit = {
    val text = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod(???)""".stripMargin
    }
    val result = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testInlineBlockHighlighting(): Unit =
    z {
      s"""$START${fromEffect("UIO(1)")}.$faultyMethod {
         |  val a = 1
         |  b
         |}$END""".stripMargin
    }.assertHighlighted()

  def testInlineBlockReplacement(): Unit = {
    val text = z {
      s"""${fromEffect("UIO(1)")}.$faultyMethod {
         |  val a = 1
         |  ???
         |}""".stripMargin
    }
    val result = z(s"${fromEffect("UIO(1)")}")
    testQuickFix(text, result, hint)
  }

  def testValBlockHighlighting(): Unit =
    z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |${START}foo.$faultyMethod {
         |  val a = 1
         |  ???
         |}$END""".stripMargin
    }.assertHighlighted()

  def testValBlockReplacement(): Unit = {
    val text = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod {
         |  val a = 1
         |  ???
         |}""".stripMargin
    }
    val result = z {
      s"""val foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testDefBlockHighlighting(): Unit = z {
    s"""def foo = ${fromEffect("UIO(1)")}
       |${START}foo.$faultyMethod {
       |  val a = 1
       |  ???
       |}$END""".stripMargin
  }.assertHighlighted()

  def testDefBlockReplacement(): Unit = {
    val text = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo.$faultyMethod {
         |  val a = 1
         |  ???
         |}""".stripMargin
    }
    val result = z {
      s"""def foo = ${fromEffect("UIO(1)")}
         |foo""".stripMargin
    }
    testQuickFix(text, result, hint)
  }

  def testSimpleProvideInlineHighlighting(): Unit =
    z(s"$START${fromEffect("UIO(1)")}.provide(???)$END").assertHighlighted()

  def testSimpleProvideInlineReplacement(): Unit = {
    val text   = z(s"${fromEffect("UIO(1)")}.provide(???)")
    val result = z(fromEffect("UIO(1)"))
    testQuickFix(text, result, UnnecessaryEnvProvisionInspection.hint("provide"))
  }

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"$START${fromEffect("ZIO.environment[Int]")}.$faultyMethod(???)$END").assertNotHighlighted()

  def testMethodChainNoHighlighting(): Unit =
    z(s"$START${fromEffect("UIO(1)")}.fork.repeat(Schedule.spaced(5.second).forever).provide(???)$END")
      .assertNotHighlighted()
}

class ZIOUnnecessaryEnvProvisionInspectionTest extends UnnecessaryEnvProvisionInspectionTest(identity)
class ZManagedUnnecessaryEnvProvisionInspectionTest
    extends UnnecessaryEnvProvisionInspectionTest(effect => s"ZManaged.fromEffect($effect)")
class ZStreamUnnecessaryEnvProvisionInspectionTest
    extends UnnecessaryEnvProvisionInspectionTest(effect => s"ZStream.fromEffect($effect)")
