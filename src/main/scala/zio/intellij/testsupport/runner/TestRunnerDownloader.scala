package zio.intellij.testsupport.runner

import java.net.URL
import java.nio.file.Path

import com.intellij.openapi.progress.ProcessCanceledException
import org.apache.ivy.util.{AbstractMessageLogger, MessageLogger}
import org.jetbrains.plugins.scala.{DependencyManager, DependencyManagerBase, ScalaVersion}
import org.jetbrains.plugins.scala.DependencyManagerBase.{DependencyDescription, _}
import zio.intellij.testsupport.runner.TestRunnerDownloader.DownloadProgressListener
import zio.intellij.testsupport.runner.TestRunnerDownloader.DownloadResult.{DownloadFailure, DownloadSuccess}
import zio.intellij.utils.Version

import scala.util.control.NonFatal

private[runner] class TestRunnerDownloader(progressListener: DownloadProgressListener) {

  def download(version: Version)(implicit scalaVersion: ScalaVersion): Either[DownloadFailure, DownloadSuccess] =
    try {
      val resolver             = new DependencyResolver(progressListener)
      val resolvedDependencies = resolver.resolve(dependencies(version.toString): _*)
      val jars: Seq[Path]      = resolvedDependencies.map(_.file.toPath)
      val urls                 = jars.map(_.toUri.toURL).toArray
      Right(DownloadSuccess(version, scalaVersion, urls))
    } catch {
      case e: ProcessCanceledException => throw e
      case NonFatal(e)                 => Left(DownloadFailure(version, scalaVersion, e))
    }

  private def dependencies(version: String)(implicit scalaVersion: ScalaVersion): Seq[DependencyDescription] =
    List("dev.zio" %% s"zio-test-intellij" % version)

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
