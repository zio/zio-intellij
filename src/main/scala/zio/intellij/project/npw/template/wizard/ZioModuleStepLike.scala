// DO NOT RENAME THE PACKAGE
package org.jetbrains.sbt.project.template.wizard

import com.intellij.openapi.Disposable
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.ui.components.JBCheckBox
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.extensions.{applyTo, ObjectExt}
import org.jetbrains.plugins.scala.project.Versions
import org.jetbrains.sbt.project.template.SComboBox
import zio.intellij.ZioIcon
import zio.intellij.utils.{DownloadUtil, ZioVersion}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.JLabel
import scala.collection.immutable.ListSet

trait ZioModuleStepLike extends SbtModuleStepLike {
  protected var selectedZioVersion: Option[String]

  protected val defaultAvailableZioVersions: Versions

  private val availableZioVersions: AtomicReference[Option[Versions]] = new AtomicReference(None)

  private val isZioVersionManuallySelected: AtomicBoolean = new AtomicBoolean(false)

  private val isZioLoading = new AtomicBoolean(false)

  protected lazy val zioVersionComboBox: SComboBox[String] =
    createSComboBoxWithSearchingListRenderer(ListSet(defaultAvailableZioVersions.versions: _*), None, isZioLoading)

  private def downloadAvailableVersions(disposable: Disposable): Unit = {
    val zioDownloadVersions: ProgressIndicator => Versions = indicator => {
      val scalaVersion =
        selections.scalaVersion.flatMap(ScalaVersion.fromString).getOrElse(ScalaVersion.Latest.Scala_2_13)
      DownloadUtil.downloadZioVersions(scalaVersion, canBeCanceled = true, Some(indicator))
    }
    downloadVersionsAsynchronously(isZioLoading, disposable, zioDownloadVersions, "ZIO") { v =>
      availableZioVersions.set(v.toOption)
      updateSelectionsAndElementsModelForZio(v)
    }
  }

  protected def updateSelectionsAndElementsModelForZio(zioVersions: Versions): Unit = {
    if (!isZioVersionManuallySelected.get()) {
      selectedZioVersion = None
      availableZioVersions.set(Some(zioVersions))
    }
    zioVersionComboBox.updateComboBoxModel(zioVersions.versions.toArray, selectedZioVersion)
  }

  protected val zioLabel: JLabel = {
    val label = new JLabel("ZIO")
    label.setIcon(ZioIcon)
    label.setDisplayedMnemonic('Z')
    label

  }

  protected val includeZioTestCheckbox: JBCheckBox = applyTo(new JBCheckBox("Include 'zio-test'"))(
    _.setToolTipText("Includes the ZIO Test library and configuration")
  )

  override def initSelectionsAndUi(contextDisposable: Disposable): Unit = {
    super.initSelectionsAndUi(contextDisposable)
    includeZioTestCheckbox.setSelected(true)
    downloadAvailableVersions(contextDisposable)
  }
}
