// DO NOT RENAME THE PACKAGE
package org.jetbrains.sbt.project.template.wizard.buildSystem

import com.intellij.ide.JavaUiBundle
import com.intellij.ide.projectWizard.NewProjectWizardCollector.BuildSystem.{INSTANCE => BSLog}
import com.intellij.ide.wizard.AbstractNewProjectWizardStep
import com.intellij.openapi.GitRepositoryInitializer
import com.intellij.openapi.externalSystem.model.ExternalSystemDataKeys
import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManagerImpl
import com.intellij.openapi.module.{ModuleManager, StdModuleTypes}
import com.intellij.openapi.observable.properties.{GraphProperty, ObservableProperty, PropertyGraph}
import com.intellij.openapi.observable.util.BindUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.impl.DependentSdkType
import com.intellij.openapi.projectRoots.{JavaSdkType, Sdk, SdkTypeId}
import com.intellij.openapi.roots.ui.configuration.{JdkComboBox, ProjectStructureConfigurable}
import com.intellij.openapi.ui.ValidationInfo
import com.intellij.ui.UIBundle
import com.intellij.ui.components.JBTextField
import com.intellij.ui.dsl.builder._
import com.intellij.ui.dsl.gridLayout.HorizontalAlign
import com.intellij.ui.layout.ValidationInfoBuilder
import kotlin.Unit.{INSTANCE => KUnit}
import org.jetbrains.annotations.TestOnly
import org.jetbrains.plugins.scala.extensions.{ObjectExt, ToNullSafe}
import org.jetbrains.plugins.scala.project.Versions
import org.jetbrains.plugins.scala.util.ui.extensions.JComboBoxOps
import org.jetbrains.sbt.project.template.SbtModuleBuilderSelections
import org.jetbrains.sbt.project.template.wizard.kotlin_interop.{ComboBoxKt_Wrapper, JdkComboBoxKt_Interop}
import org.jetbrains.sbt.project.template.wizard.{ScalaNewProjectWizardStep, ZioModuleStepLike}
import zio.intellij.project.npw.template.wizard.ZioProjectBuilder
import zio.intellij.utils.ZioVersion
import zio.intellij.utils.ZioVersion.ZIO

import javax.swing.JLabel
import scala.annotation.nowarn

// copied from SbtScalaNewProjectWizardStep

//noinspection ApiStatus,UnstableApiUsage
final class ZioNewProjectWizardStep(parent: ScalaNewProjectWizardStep)
    extends AbstractNewProjectWizardStep(parent)
    with SbtScalaNewProjectWizardData
    with ScalaGitNewProjectWizardData
    with ScalaSampleCodeNewProjectWizardData
    with ZioModuleStepLike {

  @inline private def propertyGraph: PropertyGraph = getPropertyGraph

  private var sdkComboBox: Cell[JdkComboBox]            = _
  private val sdkProperty: GraphProperty[Sdk]           = propertyGraph.property(null)
  private val moduleNameProperty: GraphProperty[String] = propertyGraph.lazyProperty(() => parent.getName)

  private val addSampleCodeProperty: GraphProperty[java.lang.Boolean] = propertyGraph.property(java.lang.Boolean.FALSE)
  BindUtil.bindBooleanStorage(addSampleCodeProperty, "NewProjectWizard.addSampleCodeState")
  @TestOnly override private[project] def setAddSampleCode(value: java.lang.Boolean): Unit =
    addSampleCodeProperty.set(value)
  private def needToAddSampleCode: Boolean = addSampleCodeProperty.get()

  private val gitProperty: GraphProperty[java.lang.Boolean] = propertyGraph.property(java.lang.Boolean.FALSE)
  BindUtil.bindBooleanStorage(gitProperty, "NewProjectWizard.gitState")
  @TestOnly override private[project] def setGit(value: java.lang.Boolean): Unit = gitProperty.set(value)
  private def isGitRepository: Boolean =
    Option(GitRepositoryInitializer.getInstance()).isDefined && gitProperty.get()

  @TestOnly override private[project] def setScalaVersion(version: String): Unit =
    scalaVersionComboBox.setSelectedItemEnsuring(version)
  @TestOnly override private[project] def setSbtVersion(version: String): Unit =
    sbtVersionComboBox.setSelectedItemEnsuring(version)
  @TestOnly override private[project] def setPackagePrefix(prefix: String): Unit =
    packagePrefixTextField.setText(prefix)

  def getSdk: Sdk           = sdkProperty.get()
  def getModuleName: String = moduleNameProperty.get()

  override protected val selections: SbtModuleBuilderSelections = SbtModuleBuilderSelections.default
  override protected var selectedZioVersion: Option[String]     = None

  override protected lazy val defaultAvailableScalaVersions: Versions = Versions.Scala.allHardcodedVersions
  override protected lazy val defaultAvailableSbtVersions: Versions   = Versions.SBT.allHardcodedVersions
  override protected lazy val defaultAvailableSbtVersionsForScala3: Versions =
    Versions.SBT.sbtVersionsForScala3(defaultAvailableSbtVersions)
  override protected lazy val defaultAvailableZioVersions: Versions =
    Versions(ZIO.`2.x.latest`.toString, ZIO.`2.x.latest`.toString :: ZIO.`1.x.latest`.toString :: Nil)

  locally {
    moduleNameProperty.dependsOn(
      parent.getNameProperty: ObservableProperty[String],
      (() => parent.getName): kotlin.jvm.functions.Function0[_ <: String]
    )

    getData.putUserData(SbtScalaNewProjectWizardData.KEY, this)
    getData.putUserData(ScalaGitNewProjectWizardData.KEY, this)
    getData.putUserData(ScalaSampleCodeNewProjectWizardData.KEY, this)
  }

  override def setupProject(project: Project): Unit = {
    val zioVersion = ZioVersion.parseUnsafe(selectedZioVersion.getOrElse(ZIO.`2.x.latest`.toString))
    val builder = new ZioProjectBuilder(
      ZioProjectBuilder.Selections(
        sbtVersion = selections.sbtVersion,
        scalaVersion = selections.scalaVersion,
        zioVersion = Some(zioVersion.toString),
        downloadScalaSdkSources = selections.downloadScalaSdkSources,
        downloadSbtSources = selections.downloadSbtSources,
        includeZioTest = includeZioTestCheckbox.isSelected,
        includeHelloWorld = needToAddSampleCode,
        packagePrefix = selections.packagePrefix
      )
    )
    builder.setName(getModuleName)
    val projectRoot = getContext.getProjectDirectory.toAbsolutePath
    builder.setContentEntryPath(projectRoot.toString)

    setProjectOrModuleSdk(project, parent, builder, Option(getSdk))

    ExternalProjectsManagerImpl.setupCreatedProject(project)

    /**
     * NEWLY_CREATED_PROJECT must be set up to prevent the call of markDirtyAllExternalProjects in ExternalProjectsDataStorage#load.
     * As a result, NEWLY_IMPORTED_PROJECT must also be set to keep the same behaviour as before in ExternalSystemStartupActivity.kt:48 (do not call ExternalSystemUtil#refreshProjects).
     * Similar thing is done in AbstractGradleModuleBuilder#setupModule
     */
    project.putUserData(ExternalSystemDataKeys.NEWLY_CREATED_PROJECT, java.lang.Boolean.TRUE)
    project.putUserData(ExternalSystemDataKeys.NEWLY_IMPORTED_PROJECT, java.lang.Boolean.TRUE)

    if (isGitRepository) addGitIgnore(project, projectRoot.toString)

    builder.commit(project)
  }

  override def setupUI(panel: Panel): Unit = {
    panel.row(
      JavaUiBundle.message("label.project.wizard.new.project.jdk"),
      (row: Row) => {
        val javaSdkFilter: kotlin.jvm.functions.Function1[SdkTypeId, java.lang.Boolean] =
          (it: SdkTypeId) => it.isInstanceOf[JavaSdkType] && !it.is[DependentSdkType]
        sdkComboBox = JdkComboBoxKt_Interop.sdkComboBox(
          row,
          getContext,
          sdkProperty,
          StdModuleTypes.JAVA.getId,
          javaSdkFilter,
          null,
          null,
          null,
          null
        )
        ComboBoxKt_Wrapper.columns(sdkComboBox, TextFieldKt.COLUMNS_MEDIUM)
        KUnit
      }
    )

    panel.row(
      sbtLabelText,
      (row: Row) => {
        row.layout(RowLayout.PARENT_GRID)
        row.cell(sbtVersionComboBox).horizontalAlign(HorizontalAlign.FILL): @nowarn("cat=deprecation")
        row.cell(downloadSbtSourcesCheckbox)
        KUnit
      }
    )

    panel.row(
      scalaLabelText,
      (row: Row) => {
        row.layout(RowLayout.PARENT_GRID)
        row.cell(scalaVersionComboBox).horizontalAlign(HorizontalAlign.FILL): @nowarn("cat=deprecation")
        row.cell(downloadScalaSourcesCheckbox)
        KUnit
      }
    )

    panel.row(
      zioLabel,
      (row: Row) => {
        row.layout(RowLayout.PARENT_GRID)
        row.cell(zioVersionComboBox).horizontalAlign(HorizontalAlign.FILL): @nowarn("cat=deprecation")
        row.cell(includeZioTestCheckbox)
        KUnit
      }
    )

    setupPackagePrefixUI(panel)

    panel
      .row(
        null: JLabel,
        (row: Row) => {
          val cb = row.checkBox("""Create a "Hello World" main app""")
          ButtonKt.bindSelected(
            cb,
            addSampleCodeProperty: com.intellij.openapi.observable.properties.ObservableMutableProperty[
              java.lang.Boolean
            ]
          )
          ButtonKt.whenStateChangedFromUi(
            cb,
            null,
            value => {
              BSLog.logAddSampleCodeChanged(parent, value): @nowarn("cat=deprecation")
              KUnit
            }
          )
          KUnit
        }
      )
      .topGap(TopGap.SMALL)

    panel.collapsibleGroup(
      UIBundle.message("label.project.wizard.new.project.advanced.settings"),
      true,
      (panel: Panel) => {
        if (getContext.isCreatingNewProject) {
          panel.row(
            UIBundle.message("label.project.wizard.new.project.module.name"),
            (row: Row) => {
              val validator: kotlin.jvm.functions.Function2[ValidationInfoBuilder, JBTextField, ValidationInfo] =
                (builder, field) => {
                  validateModuleName(builder, field)
                }
              TextFieldKt
                .bindText(
                  row.textField,
                  moduleNameProperty: com.intellij.openapi.observable.properties.ObservableMutableProperty[String]
                )
                .horizontalAlign(HorizontalAlign.FILL)
                .validationOnInput(validator)
                .validationOnApply(validator): @nowarn("cat=deprecation")
              KUnit
            }
          )
        }
        KUnit
      }
    )

    initSelectionsAndUi(getContext.getDisposable)
  }

  private def validateModuleName(builder: ValidationInfoBuilder, field: JBTextField): ValidationInfo = {
    val moduleName = field.getText
    val project    = getContext.getProject
    if (moduleName.isEmpty)
      builder.error(JavaUiBundle.message("module.name.location.dialog.message.enter.module.name"))
    else if (project == null)
      null
    else {
      // Name uniqueness
      val model = ProjectStructureConfigurable
        .getInstance(project)
        .nullSafe
        .map(_.getContext)
        .map(_.getModulesConfigurator)
        .map(_.getModuleModel)
        .orNull

      val module =
        if (model == null)
          ModuleManager.getInstance(project).findModuleByName(moduleName)
        else
          model.findModuleByName(moduleName)

      if (module != null)
        builder.error(
          JavaUiBundle.message("module.name.location.dialog.message.module.already.exist.in.project", moduleName)
        )
      else
        null
    }
  }
}
