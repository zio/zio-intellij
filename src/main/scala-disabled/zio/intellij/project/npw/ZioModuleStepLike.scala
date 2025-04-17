// DO NOT RENAME THE PACKAGE
package org.jetbrains.sbt.project.template.wizard

import com.intellij.openapi.Disposable
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.ui.components.JBCheckBox
import com.intellij.ui.dsl.builder.{Panel, Row, RowLayout}
import com.intellij.ui.dsl.gridLayout.HorizontalAlign
import kotlin.Unit.{INSTANCE => KUnit}
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.extensions.{ObjectExt, applyTo}
import org.jetbrains.plugins.scala.project.Versions
import org.jetbrains.sbt.project.template.SComboBox
import zio.intellij.ZioIcon
import zio.intellij.utils.DownloadUtil

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.JLabel
import scala.annotation.nowarn
import scala.collection.immutable.ListSet

trait ZioModuleStepLike extends SbtModuleStepLike {
  protected var selectedZioVersion: Option[String]

  protected val defaultAvailableZioVersions: Versions

  private val availableZioVersions: AtomicReference[Option[Versions]] = new AtomicReference(None)

  private val isZioVersionManuallySelected: AtomicBoolean = new AtomicBoolean(false)

  //
  // Raw UI elements
  //

  private val isZioLoading = new AtomicBoolean(false)
  private lazy val zioVersionComboBox: SComboBox[String] =
    createSComboBoxWithSearchingListRenderer(ListSet(defaultAvailableZioVersions.versions: _*), None, isZioLoading)


  private def downloadZioVersions(disposable: Disposable): Unit = {
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

  protected def setUpZioUI(panel: Panel) = {
    panel.row(
      zioLabel,
      (row: Row) => {
        row.layout(RowLayout.PARENT_GRID)
        row.cell(zioVersionComboBox).horizontalAlign(HorizontalAlign.FILL): @nowarn("cat=deprecation")
        row.cell(includeZioTestCheckbox)
        KUnit
      }
    )
  }

  protected val includeZioTestCheckbox: JBCheckBox = applyTo(new JBCheckBox("Include 'zio-test'"))(
    _.setToolTipText("Includes the ZIO Test library and configuration"),
    _.setSelected(true)
  )

  override def initSelectionsAndUi(contextDisposable: Disposable): Unit = {
    super.initSelectionsAndUi(contextDisposable)
    _initSelectionsAndUi
    downloadZioVersions(contextDisposable)
  }

  private lazy val _initSelectionsAndUi: Unit = {
    initUiElementsListeners()
  }

  private def initUiElementsListeners() = {
    zioVersionComboBox.addActionListener { _ =>
      isZioVersionManuallySelected.set(true)
      selectedZioVersion = zioVersionComboBox.getSelectedItemTyped
    }
  }

}
