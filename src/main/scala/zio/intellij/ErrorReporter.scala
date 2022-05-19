package zio.intellij

import com.intellij.diagnostic.LogMessage
import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.ide.{BrowserUtil, DataManager, IdeBundle}
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.application.ex.ApplicationInfoEx
import com.intellij.openapi.diagnostic.SubmittedReportInfo.SubmissionStatus
import com.intellij.openapi.diagnostic.{ErrorReportSubmitter, IdeaLoggingEvent, SubmittedReportInfo}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.SystemInfo
import com.intellij.util.Consumer
import zio.intellij.ErrorReporter.reportErrorOnGithub
import zio.intellij.utils.{ProjectSyntax, StringBuilderSyntax}

import java.awt.Component
import java.net.URLEncoder
import scala.util.Try

final class ErrorReporter extends ErrorReportSubmitter {
  override def getReportActionText: String = "Report on GitHub (opens a browser)"

  override def submit(
    events: Array[IdeaLoggingEvent],
    additionalInfo: String,
    parentComponent: Component,
    consumer: Consumer[_ >: SubmittedReportInfo]
  ): Boolean = {
    val project = CommonDataKeys.PROJECT.getData(DataManager.getInstance().getDataContext(parentComponent))

    val errors = events
      .map(_.getData)
      .collect {
        case d: LogMessage => d.getThrowableText
      }
      .toList

    try reportErrorOnGithub(
      errors.headOption.map(e => "Unhandled exception: " + e.split("\n", 2).head).getOrElse("An unhandled exception occurred in the plugin"),
      "The following exceptions(s) occurred in the ZIO for IntelliJ plugin:",
      Option(additionalInfo),
      errors,
      "plugin-exception",
      project
    )
    finally consumer.consume(new SubmittedReportInfo(SubmissionStatus.NEW_ISSUE))
    true
  }
}
object ErrorReporter {
  def reportErrorOnGithub(
    title: String,
    header: String,
    details: Option[String],
    errors: List[String],
    label: String,
    project: Project
  ) = {
    def enc(s: String) =
      URLEncoder.encode(s, "UTF-8")

    // (ノಠ益ಠ)ノ彡┻━┻
    val sb = new StringBuilder()
    sb.appendLine(header).appendLine
    sb.appendLine("```")
    sb.appendLine(errors.mkString("---\n"))
    sb.appendLine("```")
    details.foreach { s =>
      sb.appendLine
      sb.appendLine("### Extra details:")
      sb.appendLine(s)
    }
    appendAdditionalInformation(sb, project)

    val body = enc(sb.toString())

    BrowserUtil.browse(
      s"https://github.com/zio/zio-intellij/issues/new?title=${enc(title)}&labels=${enc(label)}&body=$body"
    )
  }

  private def appendAdditionalInformation(sb: StringBuilder, project: Project) = {
    val versions = project.versions
    sb.appendLine("### Additional information:")
    sb.appendLine("<details>")
    sb.appendLine
    sb.appendLine(s"ZIO plugin version: ${pluginVersion("zio.intellij.zio-intellij").getOrElse("unknown")}")
    sb.appendLine(s"Scala plugin version: ${pluginVersion("org.intellij.scala").getOrElse("unknown")}")
    sb.appendLine(s"ZIO version(s): ${versions.map(_._1).mkString(", ")}")
    sb.appendLine(s"Scala version(s): ${versions.map(_._2.minor).mkString(", ")}")
    sb.appendLine(s"sbt version: ${Try(project.sbtVersion).getOrElse("N/A")}")
    val appInfo = ApplicationInfoEx.getInstanceEx
    sb.appendLine(s"IntelliJ version: ${appInfo.getFullApplicationName}")
    sb.appendLine(s"${IdeBundle.message("about.box.build.number", appInfo.getBuild.asString)}")
    sb.append("Java version: ")
    val properties = System.getProperties
    sb.appendLine(properties.getProperty("java.runtime.version", properties.getProperty("java.version", "unknown")))
    sb.append("Operating System: ")
    sb.appendLine(
      s"${properties.getProperty("os.arch", "")} ${SystemInfo.OS_NAME + " (" + SystemInfo.OS_VERSION + ", " + SystemInfo.OS_ARCH + ")"}"
    )
    sb.append("JVM version: ")
    sb.appendLine(
      s"${properties.getProperty("java.vm.name", "unknown")} ${properties.getProperty("java.vendor", "unknown")}"
    )
    sb.appendLine("</details>")
  }

  private def pluginVersion(id: String): Option[String] =
    PluginManagerCore.getPlugins
      .find(_.getPluginId.getIdString == id)
      .map(_.getVersion)
}
