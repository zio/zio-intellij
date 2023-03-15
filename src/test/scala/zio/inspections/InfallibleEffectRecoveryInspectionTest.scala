package zio.inspections

import zio.intellij.inspections.mistakes.InfallibleEffectRecoveryInspection

abstract class InfallibleEffectRecoveryInspectionTest(fromEffect: String => String)
    extends ZScalaInspectionTest[InfallibleEffectRecoveryInspection] {
  private val faultyMethod = "orElse"

  override protected val description = InfallibleEffectRecoveryInspection.description(".*")
  private val hint                   = InfallibleEffectRecoveryInspection.hint(faultyMethod)

  override protected def descriptionMatches(s: String): Boolean = s != null && s.matches(description)

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

  def testFallibleEffectNoHighlighting(): Unit =
    z(s"$START${fromEffect("Task(1)")}.$faultyMethod(???)$END").assertNotHighlighted()
}

class InfallibleZIORecoveryInspectionTest extends InfallibleEffectRecoveryInspectionTest(identity) {
  def testInfallibleFoldNoHighlighting(): Unit =
    z(s"${START}ZIO.succeed(1).fold(???, ???)$END").assertNotHighlighted()

  def testInfallibleFoldTraceMNoHighlighting(): Unit =
    z(s"${START}ZIO.succeed(1).foldTraceM(???, ???)$END").assertNotHighlighted()

  def testInfallibleFoldMNoHighlighting(): Unit =
    z(s"${START}ZIO.succeed(1).foldM(???, ???)$END").assertNotHighlighted()

  def testInfallibleMapBothNoHighlighting(): Unit =
    z(s"${START}ZIO.succeed(1).mapBoth(???, ???)$END").assertNotHighlighted()

  def testInfallibleBimapNoHighlighting(): Unit =
    z(s"${START}ZIO.succeed(1).bimap(???, ???)$END").assertNotHighlighted()
}
class InfallibleZManagedRecoveryInspectionTest
    extends InfallibleEffectRecoveryInspectionTest(effect => s"ZManaged.fromEffect($effect)")
class InfallibleZStreamRecoveryInspectionTest
    extends InfallibleEffectRecoveryInspectionTest(effect => s"ZStream.fromEffect($effect)")
