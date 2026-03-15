package zio.intellij.startup

import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.ide.util.PropertiesComponent
import com.intellij.notification._
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.extensions.PluginId
import com.intellij.openapi.project.Project
import com.intellij.openapi.updateSettings.impl.{UpdateChecker, UpdateSettings}
import org.jetbrains.plugins.scala.startup.ProjectActivity

import java.util.concurrent.atomic.AtomicBoolean

final class ZioPluginVersionCheckActivity extends ProjectActivity {
  override def execute(project: Project): Unit =
    ZioPluginVersionCheckActivity.checkAndNotify(project)
}

object ZioPluginVersionCheckActivity {

  private val DontShowAgainKey   = "zio.intellij.scala.plugin.mismatch.dismissed"
  private val LastWarnedComboKey = "zio.intellij.scala.plugin.mismatch.last.warned.combo"

  private val ZioPluginId   = PluginId.getId("zio.intellij.zio-intellij")
  private val ScalaPluginId = PluginId.getId("org.intellij.scala")

  private val ScalaPluginStableUrl = "https://plugins.jetbrains.com/plugin/1347"
  private val ZioPluginEapUrl      = "https://plugins.jetbrains.com/plugins/eap/13820"

  private val notificationShown = new AtomicBoolean(false)

  private def checkAndNotify(project: Project): Unit = {
    if (!notificationShown.compareAndSet(false, true)) return

    if (ApplicationManager.getApplication.isUnitTestMode) return
    if (isScalaPluginStable || !isZioPluginStable) return
    if (PropertiesComponent.getInstance().isTrueValue(DontShowAgainKey)) return
    if (isAlreadyWarnedForCurrentCombo) return

    markWarnedForCurrentCombo()

    val notification = NotificationGroupManager
      .getInstance()
      .getNotificationGroup("ZIO Plugin Version Mismatch")
      .createNotification(
        "ZIO for IntelliJ: Scala plugin version mismatch",
        "You are using a <b>stable</b> version of <b>ZIO for IntelliJ</b> " +
          "together with an <b>EAP/Nightly</b> version of the Scala plugin. " +
          "This combination may cause unexpected issues.",
        NotificationType.WARNING
      )

    notification.addAction(new NotificationAction("Switch to ZIO IntelliJ EAP") {
      override def actionPerformed(e: AnActionEvent, n: Notification): Unit = {
        switchToEapChannel()
        n.expire()
      }
    })

    notification.addAction(new NotificationAction("Don't show again") {
      override def actionPerformed(e: AnActionEvent, n: Notification): Unit = {
        PropertiesComponent.getInstance().setValue(DontShowAgainKey, true)
        n.expire()
      }
    })

    notification.notify(project)
  }

  private def switchToEapChannel(): Unit = {
    UpdateSettings.getInstance().getStoredPluginHosts.add(ZioPluginEapUrl)
    UpdateChecker.updateAndShowResult()
  }

  /**
   * Stable releases are published without ZIO_INTELLIJ_BUILD_NUMBER, which defaults to "0".
   */
  private def isZioPluginStable: Boolean =
    Option(PluginManagerCore.getPlugin(ZioPluginId))
      .flatMap(p => parseBuildComponent(p.getVersion))
      .contains(0)

  private def parseBuildComponent(version: String): Option[Int] =
    version.split("\\.") match {
      case Array(_, _, _, build) => build.toIntOption
      case _                     => None
    }

  // seems to be the only way to determine Intellij Scala build
  private def isScalaPluginStable: Boolean =
    UpdateSettings.getInstance().getStoredPluginHosts.contains(ScalaPluginStableUrl)

  private def isAlreadyWarnedForCurrentCombo: Boolean =
    PropertiesComponent.getInstance().getValue(LastWarnedComboKey, "") == currentVersionCombo

  private def markWarnedForCurrentCombo(): Unit =
    PropertiesComponent.getInstance().setValue(LastWarnedComboKey, currentVersionCombo)

  private def currentVersionCombo: String = {
    val zioVersion   = Option(PluginManagerCore.getPlugin(ZioPluginId)).map(_.getVersion).getOrElse("unknown")
    val scalaVersion = Option(PluginManagerCore.getPlugin(ScalaPluginId)).map(_.getVersion).getOrElse("unknown")
    s"$zioVersion|$scalaVersion"
  }

}
