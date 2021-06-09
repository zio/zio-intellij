package zio.intellij.testsupport.runner

import com.intellij.ide.BrowserUtil
import com.intellij.notification._
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.project.Project
import org.jetbrains.annotations.NonNls
import zio.intellij.testsupport.runner.TestRunnerNotifications.{displayError, displayInfo}
import zio.intellij.testsupport.runner.TestRunnerResolveService.ResolveError
import zio.intellij.utils.{ProjectSyntax, ScalaVersionHack, Version}
import zio.intellij.{ErrorReporter, ZioIcon}

import javax.swing.event.HyperlinkEvent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private[runner] final class TestRunnerProjectNotification(private val project: Project) {
  def init(): Unit =
    if (shouldSuggestTestRunner(project)) {
      createNotification.notify(project)
    }

  private def shouldSuggestTestRunner(project: Project, downloadIfMissing: Boolean = false): Boolean =
    project.versions.foldLeft(false) {
      case (flag, (version, scalaVersion)) =>
        flag | TestRunnerResolveService.instance
          .resolve(version, scalaVersion, downloadIfMissing)
          .toOption
          .isEmpty
    }

  //noinspection HardCodedStringLiteral
  private def href(ref: String, text: String): String = s"""<a href="$ref">$text</a>"""
  @NonNls private val Nbsp                            = "&nbsp;"

  private val listener: NotificationListener = (notification: Notification, link: HyperlinkEvent) => {
    link.getDescription match {
      case "download" => downloadTestRunner(notification)
      case "learn_more" =>
        BrowserUtil.open("https://plugins.jetbrains.com/plugin/13820-zio-for-intellij/zio-test-runner")
      case _ =>
    }
  }

  private def downloadTestRunner(notification: Notification): Unit = {
    val tasks = project.versions.map {
      case (version, scalaVersion) =>
        TestRunnerResolveService.instance
          .resolveAsync(
            version,
            scalaVersion,
            project
          )
    }

    Future.sequence(tasks).foreach { results =>
      if (results.forall(_.isRight)) displayInfo("ZIO Test runner was downloaded successfully!")
      else
        displayError(
          "Unable to download one or more required files. Please try again, or report this issue on GitHub, if the problem persists.",
          Seq(
            new NotificationAction("Try again") {
              override def actionPerformed(e: AnActionEvent, notification: Notification): Unit =
                downloadTestRunner(notification)
            },
            new NotificationAction("Report on GitHub (opens a browser)") {
              override def actionPerformed(e: AnActionEvent, notification: Notification): Unit = {
                val errors = results.collect {
                  case Left(error) =>
                    error match {
                      case ResolveError.NotFound(version, scalaVersion) =>
                        s"Not found: zio-test-intellij_${scalaVersion.versionStr}:$version"
                      case ResolveError.DownloadInProgress(version, scalaVersion) =>
                        s"Download in progress: zio-test-intellij_${scalaVersion.versionStr}:$version"
                      case ResolveError.DownloadError(version, scalaVersion, cause) =>
                        s"""Download error: zio-test-intellij_${scalaVersion.versionStr}:$version"
                           |Cause:
                           |${cause.toString}""".stripMargin
                      case ResolveError.UnknownError(version, scalaVersion, cause) =>
                        s"""Unknown error: zio-test-intellij_${scalaVersion.versionStr}:$version"
                           |Cause:
                           |${cause.toString}""".stripMargin
                    }
                }

                try ErrorReporter.reportErrorOnGithub(
                  "Problem downloading the test runner",
                  "The following error(s) occurred while downloading the ZIO Test runner files:",
                  None,
                  errors,
                  project.versions,
                  "test-runner"
                )
                finally notification.expire()
              }
            }
          ),
          icon = Some(ZioIcon)
        )
    }
    notification.expire()
  }

  private def createNotification: Notification =
    suggesterNotificationGroup
      .createNotification(
        "Enable the integrated ZIO Test runner",
        href("download", "Download ZIO Test runner") + Nbsp * 6 +
          href("learn_more", "Learn more..."),
        NotificationType.INFORMATION,
        listener
      )
      .setIcon(ZioIcon)

  private val suggesterNotificationGroup: NotificationGroup =
    NotificationGroupManager.getInstance.getNotificationGroup("Test Runner Download")
}
