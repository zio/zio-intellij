package zio.intellij.testsupport

import com.intellij.execution.configurations.JavaParameters
import com.intellij.openapi.util.SystemInfo.{isLinux, isMac, isWindows}
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.util.Markers
import org.junit.Assert.assertArrayEquals

import java.net.URL
import java.nio.file.Paths
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class WindowsZTestRunConfigurationTest
    extends ZTestRunConfigurationTestBase(isWindows)(
      path =
        new URL("file:/C:/Users/taka/.ivy2/cache/dev.zio/zio-test-intellij_2.12/jars/zio-test-intellij_2.12-1.0.4.jar"),
      assertPath = """C:\Users\taka\.ivy2\cache\dev.zio\zio-test-intellij_2.12\jars\zio-test-intellij_2.12-1.0.4.jar"""
    )

class LinuxZTestRunConfigurationTest
    extends ZTestRunConfigurationTestBase(isLinux)(
      path =
        new URL("file:/usr/home/taka/.ivy2/cache/dev.zio/zio-test-intellij_2.12/jars/zio-test-intellij_2.12-1.0.4.jar"),
      assertPath = """/usr/home/taka/.ivy2/cache/dev.zio/zio-test-intellij_2.12/jars/zio-test-intellij_2.12-1.0.4.jar"""
    )

class MacOsZTestRunConfigurationTest
    extends ZTestRunConfigurationTestBase(isMac)(
      path =
        new URL("file:/Users/taka/.ivy2/cache/dev.zio/zio-test-intellij_2.12/jars/zio-test-intellij_2.12-1.0.4.jar"),
      assertPath = """/Users/taka/.ivy2/cache/dev.zio/zio-test-intellij_2.12/jars/zio-test-intellij_2.12-1.0.4.jar"""
    )

abstract class ZTestRunConfigurationTestBase(shouldRun: Boolean)(
  path: URL,
  assertPath: String
) extends ScalaLightCodeInsightFixtureTestAdapter
    with Markers {
  def rebuildList(input: List[String]): List[String] = {
    val mutableList: ListBuffer[String] = ListBuffer.empty[String]
    input
      .sliding(2, 2)
      .toList
      .collect {
        case "-s" :: suite :: _       => mutableList.appendAll(Seq("-s", suite))
        case "-testName" :: test :: _ => mutableList.appendAll(Seq("-t", test))
      }
    mutableList.toList
  }

  override def shouldRunTest(): Boolean = shouldRun

  def testRunStateProvider(): Unit = {

    val paths: Seq[URL] = Seq(path)

    val javaParameters = new JavaParameters
    javaParameters.getClassPath.addAll(paths.map(p => Paths.get(p.toURI).toFile.toString).asJava)

    assertArrayEquals(
      "must have same element",
      javaParameters.getClassPath.getPathList.asScala.toArray: Array[Object],
      Iterable(assertPath).toArray: Array[Object]
    )
  }
}
