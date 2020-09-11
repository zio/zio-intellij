package zio.inspections

import com.intellij.testFramework.EditorTestUtil.{SELECTION_END_TAG => END, SELECTION_START_TAG => START}
import zio.intellij.inspections.simplifications.SimplifyTapInspection

abstract class BaseSimplifyTapInspectionTest(s: String) extends ZSimplifyInspectionTest[SimplifyTapInspection] {
  override protected val hint = s"Replace with $s"
}

class SimplifyTapBothInspectionTest extends BaseSimplifyTapInspectionTest(".tapBoth") {

  def test_tap_tapError(): Unit = {
    z(s"ZIO.unit.${START}tap(_ => ZIO.unit).tapError(t => logError(t))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.tap(_ => ZIO.unit).tapError(t => logError(t))")
    val result = z(s"ZIO.unit.tapBoth(t => logError(t), _ => ZIO.unit)")
    testQuickFixes(text, result, hint)
  }

  def test_tapError_tap(): Unit = {
    z(s"ZIO.unit.${START}tapError(t => logError(t)).tap(_ => ZIO.unit)$END").assertHighlighted()
    val text   = z(s"ZIO.unit.tapError(t => logError(t)).tap(_ => ZIO.unit)")
    val result = z(s"ZIO.unit.tapBoth(t => logError(t), _ => ZIO.unit)")
    testQuickFixes(text, result, hint)
  }
}

class SimplifyTapErrorInspectionTest extends BaseSimplifyTapInspectionTest(".tapError") {

  def test_catchAll_zipRight_ZIO_fail(): Unit = {
    z(s"ZIO.unit.${START}catchAll(ex => logError(ex) *> ZIO.fail(ex))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.catchAll(ex => logError(ex) *> ZIO.fail(ex))")
    val result = z(s"ZIO.unit.tapError(logError)")
    testQuickFixes(text, result, hint)
  }

  def test_flatMapError_reduce_func_no_params(): Unit = {
    z(s"ZIO.unit.${START}flatMapError(a => f(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMapError(a => f(a).as(a))")
    val result = z(s"ZIO.unit.tapError(f)")
    testQuickFixes(text, result, hint)
  }

  def test_flatMapError_func_with_params(): Unit = {
    z(s"ZIO.unit.${START}flatMapError(a => f(a, 42).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMapError(a => f(a, 42).as(a))")
    val result = z(s"ZIO.unit.tapError(a => f(a, 42))")
    testQuickFixes(text, result, hint)
  }

  def test_flatMapError_method_invocation_on_ref(): Unit = {
    z(s"ZIO.unit.${START}flatMapError(a => logger.log(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMapError(a => logger.log(a).as(a))")
    val result = z(s"ZIO.unit.tapError(a => logger.log(a))")
    testQuickFixes(text, result, hint)
  }

  def test_flatMapError_to_value_underscore(): Unit = {
    z(s"ZIO.unit.${START}flatMapError(a => b.as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMapError(a => b.as(a))")
    val result = z(s"ZIO.unit.tapError(_ => b)")
    testQuickFixes(text, result, hint)
  }

  def test_flatMapError_to_qualified_value_underscore(): Unit = {
    z(s"ZIO.unit.${START}flatMapError(a => ZIO.unit.as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMapError(a => ZIO.unit.as(a))")
    val result = z(s"ZIO.unit.tapError(_ => ZIO.unit)")
    testQuickFixes(text, result, hint)
  }
}

class SimplifyTapInspectionTest extends BaseSimplifyTapInspectionTest(".tap") {

  def test_flatMap_reduce_func_no_params(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => f(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => f(a).as(a))")
    val result = z(s"ZIO.unit.tap(f)")
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_func_with_params(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => f(a, 42).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => f(a, 42).as(a))")
    val result = z(s"ZIO.unit.tap(a => f(a, 42))")
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_method_invocation_on_ref(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => logger.log(a).as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => logger.log(a).as(a))")
    val result = z(s"ZIO.unit.tap(a => logger.log(a))")
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_to_value_underscore(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => b.as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => b.as(a))")
    val result = z(s"ZIO.unit.tap(_ => b)")
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_to_qualified_value_underscore(): Unit = {
    z(s"ZIO.unit.${START}flatMap(a => ZIO.unit.as(a))$END").assertHighlighted()
    val text   = z(s"ZIO.unit.flatMap(a => ZIO.unit.as(a))")
    val result = z(s"ZIO.unit.tap(_ => ZIO.unit)")
    testQuickFixes(text, result, hint)
  }

  def test_flatMap_forkManaged(): Unit = {
    z(s"""class Server {
         |  def serve(): UIO[Unit] = ???
         |}
         |
         |val build: ZManaged[Any, Nothing, Server] = ???
         |
         |build.${START}flatMap(server => server.serve().forkManaged.as(server))$END
         |""".stripMargin).assertHighlighted()
    val text   = z(s"""class Server {
                    |  def serve(): UIO[Unit] = ???
                    |}
                    |
                    |val build: ZManaged[Any, Nothing, Server] = ???
                    |
                    |build.flatMap(server => server.serve().forkManaged.as(server))
                    |""".stripMargin)
    val result = z(s"""class Server {
                      |  def serve(): UIO[Unit] = ???
                      |}
                      |
                      |val build: ZManaged[Any, Nothing, Server] = ???
                      |
                      |build.tap(_.serve().forkManaged)
                      |""".stripMargin)
    testQuickFixes(text, result, hint)
  }
}
