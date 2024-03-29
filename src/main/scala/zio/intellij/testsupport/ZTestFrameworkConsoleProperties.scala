package zio.intellij.testsupport

import com.intellij.execution.Executor
import com.intellij.execution.process.{AnsiEscapeDecoder, ProcessOutputTypes}
import com.intellij.execution.testframework.TestConsoleProperties
import com.intellij.execution.testframework.sm.SMCustomMessagesParsing
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter
import com.intellij.util.ReflectionUtil
import jetbrains.buildServer.messages.serviceMessages._
import org.jetbrains.plugins.scala.testingSupport.test.{
  AbstractTestFramework,
  AbstractTestRunConfiguration,
  ScalaTestFrameworkConsoleProperties
}

import java.io.PrintStream
import scala.util.control.NoStackTrace

private[zio] class ZTestFrameworkConsoleProperties(configuration: AbstractTestRunConfiguration, executor: Executor)
    extends ScalaTestFrameworkConsoleProperties(configuration, "ZIO Test", executor)
    with SMCustomMessagesParsing {

  override def createTestEventsConverter(
    testFrameworkName: String,
    consoleProperties: TestConsoleProperties
  ): OutputToGeneralTestEventsConverter =
    new ZTestEventsConverter(testFrameworkName, consoleProperties)

  private class ZTestEventsConverter(testFrameworkName: String, consoleProperties: TestConsoleProperties)
      extends OutputToGeneralTestEventsConverter(testFrameworkName, consoleProperties) { self =>

    // This entire thing makes me cry :(
    // All this is needed to emit a custom TestFailedEvent that contains the expected and actual values extracted from
    // the output of a ZIO Test. This allows displaying a clickable hyperlink to see the differences in IDEA's built-in
    // diff viewer, because it assumes a JUnit-style failure reporting ("Expected:", "Actual:") which ZIO Test doesn't do.
    // Unfortunately, the visitor is both private and not overridable, so we have to resort to reflection to get to it.
    private lazy val underlyingTestVisitor = ReflectionUtil
      .findFieldInHierarchy(classOf[OutputToGeneralTestEventsConverter], _.getName == "myServiceMessageVisitor")
      .get(self)
      .asInstanceOf[ServiceMessageVisitor]

    private lazy val testVisitor = new ZTestVisitor(underlyingTestVisitor, configuration.testFramework)

    override def processServiceMessage(message: ServiceMessage, visitor: ServiceMessageVisitor): Unit =
      message.visit(testVisitor)

  }

  private class ZTestVisitor(underlying: ServiceMessageVisitor, testFramework: AbstractTestFramework)
      extends DefaultServiceMessageVisitor {
    private val regexFromHellZio1 =
      raw"\[1m.\[34m([\s\S]*).\[0m.\[0m.*\[31mwas not equal to.*\[1m.\[34m([\s\S]*?).\[0m.\[0m".r

    private val regexFromHellZio2 =
      raw"\[1m.\[34m([\s\S]*).\[0m.\[0m.*\[31mwas not equal to.\[0m..\[1m.\[34m([\s\S]*).\[0m.\[0m\s+.\[1m".r

    private val regexFromHell = testFramework match {
      case _: Zio1TestFramework => regexFromHellZio1
      case _: Zio2TestFramework => regexFromHellZio2
    }

    override def visitTestFailed(testFailed: TestFailed): Unit = {
      val details = testFailed.getStacktrace
      val tf = regexFromHell
        .findFirstMatchIn(details)
        .map { m =>
          val actual   = unescapeAnsi(m.group(1)).trim
          val expected = unescapeAnsi(m.group(2)).trim
          val ex = new Throwable with NoStackTrace {
            override def printStackTrace(s: PrintStream): Unit =
              s.println(details)

            override def toString: String = testFailed.getFailureMessage
          }
          new TestFailed(testFailed.getTestName, ex, actual, expected)
        }
        .getOrElse {
          testFailed
        }

      underlying.visitTestFailed(tf)
    }

    private def unescapeAnsi(s: String): String = {
      val builder = new StringBuilder()
      new AnsiEscapeDecoder().escapeText(s, ProcessOutputTypes.STDOUT, (text, _) => builder.append(text))
      builder.result()
    }

    override def visitTestSuiteStarted(testSuiteStarted: TestSuiteStarted): Unit =
      underlying.visitTestSuiteStarted(testSuiteStarted)

    override def visitTestSuiteFinished(testSuiteFinished: TestSuiteFinished): Unit =
      underlying.visitTestSuiteFinished(testSuiteFinished)

    override def visitTestStarted(testStarted: TestStarted): Unit =
      underlying.visitTestStarted(testStarted)

    override def visitTestFinished(testFinished: TestFinished): Unit =
      underlying.visitTestFinished(testFinished)

    override def visitTestIgnored(testIgnored: TestIgnored): Unit =
      underlying.visitTestIgnored(testIgnored)

    override def visitTestStdOut(testStdOut: TestStdOut): Unit =
      underlying.visitTestStdOut(testStdOut)

    override def visitTestStdErr(testStdErr: TestStdErr): Unit =
      underlying.visitTestStdErr(testStdErr)

    override def visitPublishArtifacts(publishArtifacts: PublishArtifacts): Unit =
      underlying.visitPublishArtifacts(publishArtifacts)

    override def visitProgressMessage(progressMessage: ProgressMessage): Unit =
      underlying.visitProgressMessage(progressMessage)

    override def visitProgressStart(progressStart: ProgressStart): Unit =
      underlying.visitProgressStart(progressStart)

    override def visitProgressFinish(progressFinish: ProgressFinish): Unit =
      underlying.visitProgressFinish(progressFinish)

    override def visitBuildStatus(buildStatus: BuildStatus): Unit =
      underlying.visitBuildStatus(buildStatus)

    override def visitBuildNumber(buildNumber: BuildNumber): Unit =
      underlying.visitBuildNumber(buildNumber)

    override def visitBuildStatisticValue(buildStatisticValue: BuildStatisticValue): Unit =
      underlying.visitBuildStatisticValue(buildStatisticValue)

    override def visitMessageWithStatus(message: Message): Unit =
      underlying.visitMessageWithStatus(message)

    override def visitBlockOpened(blockOpened: BlockOpened): Unit =
      underlying.visitBlockOpened(blockOpened)

    override def visitBlockClosed(blockClosed: BlockClosed): Unit =
      underlying.visitBlockClosed(blockClosed)

    override def visitCompilationStarted(compilationStarted: CompilationStarted): Unit =
      underlying.visitCompilationStarted(compilationStarted)

    override def visitCompilationFinished(compilationFinished: CompilationFinished): Unit =
      underlying.visitCompilationFinished(compilationFinished)

    override def visitServiceMessage(serviceMessage: ServiceMessage): Unit =
      underlying.visitServiceMessage(serviceMessage)
  }
}
