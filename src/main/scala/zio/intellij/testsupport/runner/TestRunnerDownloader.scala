package zio.intellij.testsupport.runner

import com.intellij.openapi.progress.ProcessCanceledException
import org.apache.ivy.util.{AbstractMessageLogger, MessageLogger}
import org.jetbrains.plugins.scala.DependencyManagerBase.DependencyDescription
import org.jetbrains.plugins.scala.{DependencyManagerBase, ScalaVersion}
import zio.intellij.testsupport.runner.TestRunnerDownloader.DownloadProgressListener
import zio.intellij.testsupport.runner.TestRunnerDownloader.DownloadResult.{DownloadFailure, DownloadSuccess}
import zio.intellij.utils.{ScalaVersionHack, Version}

import java.net.URL
import java.nio.file.Path
import scala.util.control.NonFatal

private[runner] class TestRunnerDownloader(progressListener: DownloadProgressListener) {

  def download(version: Version)(implicit scalaVersion: ScalaVersion): Either[DownloadFailure, DownloadSuccess] =
    try {
      val resolver             = new DependencyResolver(progressListener)
      val resolvedDependencies = resolver.resolve(dependencies(version, scalaVersion): _*)
      val jars: Seq[Path]      = resolvedDependencies.map(_.file.toPath)
      val urls                 = jars.map(_.toUri.toURL).toArray
      Right(DownloadSuccess(version, scalaVersion, urls.toIndexedSeq))
    } catch {
      case e: ProcessCanceledException => throw e
      case NonFatal(e)                 => Left(DownloadFailure(version, scalaVersion, e))
    }

  private def dependencies(version: Version, scalaVersion: ScalaVersion): Seq[DependencyDescription] =
    List(DependencyDescription("dev.zio", s"zio-test-intellij_${scalaVersion.versionStr}", version.toString))

  private class DependencyResolver(progressListener: DownloadProgressListener) extends DependencyManagerBase {
    override protected val artifactBlackList: Set[String] = Set()
    override protected val logLevel: Int                  = org.apache.ivy.util.Message.MSG_INFO
    override def createLogger: MessageLogger = new AbstractMessageLogger {
      override def doEndProgress(msg: String): Unit      = progressListener.progressUpdate(format(msg))
      override def log(msg: String, level: Int): Unit    = progressListener.progressUpdate(format(msg))
      override def rawlog(msg: String, level: Int): Unit = ()
      override def doProgress(): Unit                    = progressListener.doProgress()
    }

    @inline
    private def format(str: String): String =
      firstLine(str).trim

    private def firstLine(str: String): String = {
      val strTrimmed = str
      val newLineIdx = strTrimmed.indexOf("\n")
      strTrimmed.substring(0, if (newLineIdx != -1) newLineIdx else strTrimmed.length)
    }
  }
}
object TestRunnerDownloader {
  sealed trait DownloadResult
  object DownloadResult {
    final case class DownloadSuccess(version: Version, scalaVersion: ScalaVersion, jarUrls: Seq[URL])
        extends DownloadResult
    final case class DownloadFailure(version: Version, scalaVersion: ScalaVersion, cause: Throwable)
        extends DownloadResult
  }

  trait DownloadProgressListener {
    def progressUpdate(message: String): Unit
    def doProgress(): Unit = ()
  }

  val NoopProgressListener: DownloadProgressListener = _ => {}
}
