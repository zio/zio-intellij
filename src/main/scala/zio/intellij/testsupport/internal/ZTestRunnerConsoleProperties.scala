package zio.intellij.testsupport.internal

import com.intellij.execution.Executor
import com.intellij.execution.testframework.TestConsoleProperties
import com.intellij.execution.testframework.sm.SMCustomMessagesParsing
import com.intellij.execution.testframework.sm.runner.{ OutputToGeneralTestEventsConverter, SMTRunnerConsoleProperties }
import zio.intellij.testsupport.ZTestRunConfiguration

final private[testsupport] class ZTestRunnerConsoleProperties(
  configuration: ZTestRunConfiguration,
  executor: Executor
) extends SMTRunnerConsoleProperties(configuration, "ZIO Test", executor)
    with SMCustomMessagesParsing {

  override def isIdBasedTestTree: Boolean = false

  override def createTestEventsConverter(
    testFrameworkName: String,
    consoleProperties: TestConsoleProperties
  ): OutputToGeneralTestEventsConverter = new ZTestOutputToTestEventsConverter(testFrameworkName, consoleProperties)
}
