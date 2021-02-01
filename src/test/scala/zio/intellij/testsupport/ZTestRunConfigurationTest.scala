package zio.intellij.testsupport

import com.intellij.execution.configurations.JavaParameters

import java.net.URL
import java.nio.file.Paths
import org.jetbrains.plugins.scala.base.ScalaLightCodeInsightFixtureTestAdapter
import org.jetbrains.plugins.scala.util.Markers
import org.junit.Assert.assertArrayEquals

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class ZTestRunConfigurationTest extends ScalaLightCodeInsightFixtureTestAdapter with Markers {

  self =>
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

  def testRunStateProvider(): Unit = {

    val paths: Seq[URL] = Seq(
      new URL("file:/C:/Users/taka/.ivy2/cache/dev.zio/zio-test-intellij_2.12/jars/zio-test-intellij_2.12-1.0.4.jar")
    )

    val javaParameters = new JavaParameters
    javaParameters.getClassPath.addAll(paths.map(p => Paths.get(p.toURI).toFile.toString).asJava)

    assertArrayEquals(
      "must have same element",
      javaParameters.getClassPath.getPathList.asScala.toArray: Array[Object],
      Iterable(
        """C:\Users\taka\.ivy2\cache\dev.zio\zio-test-intellij_2.12\jars\zio-test-intellij_2.12-1.0.4.jar"""
      ).toArray: Array[Object]
    )
  }
}
