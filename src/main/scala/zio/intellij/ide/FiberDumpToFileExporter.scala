package zio.intellij.ide

import java.io.File

import com.intellij.ide.ExporterToTextFile
import com.intellij.openapi.project.Project
import zio.intellij.ide.FiberDumpToFileExporter.DEFAULT_REPORT_FILE_NAME
import zio.intellij.ui.FiberDumpRenderer
import zio.intellij.utils.jdi.fiber.model.FiberInfo

/**
 * This class was copied from the intellij-community project and modified
 *
 * https://github.com/JetBrains/intellij-community/blob/48036571856fa3482e394e9bec2a33f9876871b0/java/java-impl/src/com/intellij/unscramble/ThreadDumpPanel.java#L401
 */
class FiberDumpToFileExporter(project: Project, fiberDump: List[FiberInfo]) extends ExporterToTextFile {

  override def getReportText: String =
    fiberDump.map(FiberDumpRenderer.renderFiberInfo).mkString("\n\n")

  override def getDefaultFilePath: String =
    Option(project.getBasePath).fold("")(_ + File.separator + DEFAULT_REPORT_FILE_NAME)

  override def canExport: Boolean = fiberDump.nonEmpty
}

object FiberDumpToFileExporter {
  val DEFAULT_REPORT_FILE_NAME = "fibers_report.txt"
}
