package zio.intellij.testsupport.internal

import java.io.PrintStream
import java.util

import com.intellij.execution.process.ProcessOutputType
import com.intellij.execution.testframework.TestConsoleProperties
import com.intellij.execution.testframework.sm.runner._
import com.intellij.execution.testframework.sm.runner.events._
import com.intellij.openapi.util.Key
import jetbrains.buildServer.messages.serviceMessages.{BaseTestMessage, ServiceMessageVisitor, TestFailed}
import zio.intellij.testsupport.internal.ZTestTreeBuilder.Node.Kind._
import zio.intellij.testsupport.internal.ZTestTreeBuilder.Node.Status
import zio.intellij.testsupport.internal.ZTestTreeBuilder.TestTree
import zio.intellij.testsupport.internal.ZTestTreeBuilder.TestTree._

final private[testsupport] class ZTestOutputToTestEventsConverter(
  testFrameworkName: String,
  consoleProperties: TestConsoleProperties
) extends OutputToGeneralTestEventsConverter(
      testFrameworkName,
      consoleProperties
    ) {

  private val builder = new ZTestTreeBuilder()

  override def flushBufferOnProcessTermination(exitCode: Int): Unit = {
    onStartTesting()
    val processor = getProcessor
    processor.onTestsReporterAttached()
    builder.testTree.foreach(tree => processResults(tree, processor))
  }

  override def processConsistentText(text: String, outputType: Key[_]): Unit = {
    if (outputType == ProcessOutputType.SYSTEM) return
    // todo ugh. figure this out!
    val line = if (text.endsWith("\r\n")) text.dropRight(2) else if (text.endsWith("\n")) text.dropRight(1) else text

    builder.addLine(line)
  }

  private def processResults(tree: TestTree, processor: GeneralTestEventsProcessor): Unit = {
    def fail(label: String, message: String, isError: Boolean, details: Option[String] = None) = {
      val failed = details.fold(new TestFailed(label, message))(f => {
        val t = new Throwable() {
          override def printStackTrace(s: PrintStream): Unit =
            s.print(f)
        }
        new TestFailed(label, t)
      })
      processor.onTestFailure(new TestFailedEvent(failed, isError))
    }

    def loop(node: TestTree): Unit =
      node match {
        case SuiteNode(label, specs) =>
          processor.onSuiteStarted(new TestSuiteStartedEvent(label, null))
          specs.foreach(loop)
          processor.onSuiteFinished(new TestSuiteFinishedEvent(label))
        case TestNode(label, status, message) =>
          processor.onTestStarted(new TestStartedEvent(label, null))
          status match {
            case Status.Failed(Assertion) => fail(label, message.orNull,  false)
            case Status.Failed(Error(str))  => fail(label, str, true)
            case _                        => processor.onTestFinished(new TestFinishedEvent(label, null))
          }
      }
    loop(tree)
  }
}
