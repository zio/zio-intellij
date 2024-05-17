package zio.intellij.utils

import com.intellij.openapi.progress.ProgressIndicator
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.project.{Version, Versions}
import org.jetbrains.plugins.scala.util.HttpDownloadUtil
import zio.intellij.utils.ZioVersion.ZIO

import scala.util.Failure

object DownloadUtil {
  val hardcodedZioVersions = List(ZIO.`2.x.latest`, ZIO.`1.x.latest`)

  def downloadZioVersions(
    scalaVersion: ScalaVersion,
    canBeCanceled: Boolean,
    indicator: Option[ProgressIndicator]
  ): Versions =
    loadZioVersions(scalaVersion, canBeCanceled, indicator, propagateDownloadExceptions = true)

  private def loadZioVersions(
    scalaVersion: ScalaVersion,
    canBeCanceled: Boolean,
    indicator: Option[ProgressIndicator],
    propagateDownloadExceptions: Boolean
  ) = {
    val versionPattern = ".+>(\\d+\\.\\d+\\.\\d+(?:-\\w+)?)/<.*".r

    def extractVersions(values: Seq[String]) =
      values.collect {
        case versionPattern(number) => number
      }

    def isScala3Version(scalaVersion: String) = scalaVersion.startsWith("3")

    val versionStr = if (isScala3Version(scalaVersion.versionStr)) "3" else scalaVersion.versionStr

    def loadVersions = {
      val url   = s"https://repo1.maven.org/maven2/dev/zio/zio_$versionStr/"
      val lines = HttpDownloadUtil.loadLinesFrom(url, canBeCanceled, indicator)
      lines match {
        case Failure(exc) if propagateDownloadExceptions => throw exc
        case _                                           =>
      }
      val versionStrings = lines.fold(
        Function.const(hardcodedZioVersions.map(_.toString)),
        extractVersions
      )
      versionStrings.map(Version(_))
    }

    val versions = loadVersions.sorted.reverseIterator
      .map(_.presentation)
      .toList

    Versions(versions.headOption.getOrElse(hardcodedZioVersions.head.toString), versions)
  }
}
