package zio.intellij.project

import com.intellij.ide.util.projectWizard.{ModuleBuilder, ModuleWizardStep, SdkSettingsStep, SettingsStep}
import com.intellij.openapi.externalSystem.service.project.wizard.AbstractExternalModuleBuilder
import com.intellij.openapi.module.{JavaModuleType, ModifiableModuleModel, Module, ModuleType}
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.projectRoots.{JavaSdk, JavaSdkVersion, Sdk, SdkTypeId}
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.util.{io, text}
import com.intellij.ui.DocumentAdapter
import com.intellij.ui.components.JBTextField
import com.intellij.util.ui.UI
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.LatestScalaVersions.Scala_2_13
import org.jetbrains.plugins.scala.extensions.JComponentExt.ActionListenersOwner
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.project.{ScalaLanguageLevel, Version, Versions}
import org.jetbrains.plugins.scala.{extensions, ScalaBundle, ScalaVersion}
import org.jetbrains.sbt.project.SbtProjectSystem
import org.jetbrains.sbt.project.settings.SbtProjectSettings
import org.jetbrains.sbt.project.template.SbtModuleBuilderUtil.doSetupModule
import org.jetbrains.sbt.project.template.{SComboBox, SbtModuleBuilderUtil}
import org.jetbrains.sbt.{Sbt, SbtBundle}
import zio.intellij.testsupport.runner.TestRunnerResolveService
import zio.intellij.utils.ScalaVersionHack
import zio.intellij.utils.Version.ZIO
import zio.intellij.{utils, ZioIcon}

import java.awt.FlowLayout
import java.io.File
import javax.swing._

private[zio] class ZioProjectBuilder
    extends AbstractExternalModuleBuilder[SbtProjectSettings](SbtProjectSystem.Id, new SbtProjectSettings) {

  import Versions.{SBT => SbtKind, Scala => ScalaKind}
  import ZioProjectBuilder._

  private val selections = Selections(
    null,
    null,
    null,
    null,
    resolveClassifiers = true,
    resolveSbtClassifiers = false,
    includeZioTest = true,
    includeHelloWorld = true,
    packagePrefix = None
  )

  private lazy val scalaVersions = {
    val versions = ScalaKind.loadVersionsWithProgress()
    versions.copy(
      "2.13.6",
      versions = versions.versions.sortBy(_.startsWith("2"))
    )
  }
  private lazy val sbtVersions = SbtKind.loadVersionsWithProgress()
  private lazy val zioVersions = loadZioVersions(ScalaVersion.fromString(selections.scalaVersion).getOrElse(Scala_2_13))

  // Scala3 is only supported since sbt 1.5.0
  private val minSbtVersionForScala3 = "1.5.0"
  private lazy val sbtVersionsForScala3 = Versions(
    "1.5.3",
    sbtVersions.versions.filter(_ >= minSbtVersionForScala3)
  )

  private val minZioVersionForScala3 = "1.0.8"
  private lazy val zioVersionsForScala3 = Versions(
    ZIO.`latest-ish`.toString,
    zioVersions.versions.filter(_ >= minZioVersionForScala3)
  )

  {
    val settings = getExternalProjectSettings
    settings.setResolveJavadocs(false)
  }

  private def loadZioVersions(scalaVersion: ScalaVersion) = {
    val hardcodedVersions = ZIO.`latest-ish`.toString :: List("1.0.8", "1.0.5", "1.0.1", "1.0.0")
    val versionPattern    = ".+>(\\d+\\.\\d+\\.\\d+(?:-\\w+)?)/<.*".r

    def extractVersions(values: Seq[String]) =
      values.collect {
        case versionPattern(number) => number
      }

    val versionStr = if (isScala3Version(scalaVersion.versionStr)) "3" else scalaVersion.versionStr

    def loadVersions = {
      val url   = s"https://repo1.maven.org/maven2/dev/zio/zio_$versionStr/"
      val lines = Versions.loadLinesFrom(url)
      val versionStrings = lines.fold(
        Function.const(hardcodedVersions),
        extractVersions
      )
      versionStrings.map(Version(_))
    }

    val versions = extensions
      .withProgressSynchronously("Fetching available ZIO versions")(loadVersions)
      .sorted
      .reverse
      .map(_.presentation)

    Versions(versions.headOption.getOrElse(hardcodedVersions.head), versions)
  }

  override def getModuleType: ModuleType[_ <: ModuleBuilder] = JavaModuleType.getModuleType

  override def createModule(moduleModel: ModifiableModuleModel): Module = {
    new File(getModuleFileDirectory) match {
      case root if root.exists() =>
        {
          val settings = getExternalProjectSettings
          settings.setResolveClassifiers(selections.resolveClassifiers)
          settings.setResolveSbtClassifiers(selections.resolveSbtClassifiers)
        }

        TestRunnerResolveService
          .instance(moduleModel.getProject)
          .resolve(
            utils.Version.parseUnsafe(selections.zioVersion),
            ScalaVersion.fromString(selections.scalaVersion).getOrElse(Scala_2_13),
            downloadIfMissing = true
          )
          .get()

        createProjectTemplateIn(
          root,
          getName,
          selections.scalaVersion,
          selections.sbtVersion,
          selections.zioVersion,
          selections.sbtPlugins,
          selections.includeZioTest,
          selections.includeHelloWorld,
          selections.packagePrefix
        )

        setModuleFilePath(updateModuleFilePath(getModuleFilePath))
      case _ =>
    }

    super.createModule(moduleModel)
  }

  override def setupModule(module: Module): Unit = {
    super.setupModule(module)
    doSetupModule(module, getExternalProjectSettings, getContentEntryPath)
  }

  override def modifySettingsStep(settingsStep: SettingsStep): ModuleWizardStep = {
    //noinspection NameBooleanParameters
    {
      selections(ScalaKind) = scalaVersions
      selections(SbtKind) = sbtVersions
      selections.zioVersion = zioVersions.defaultVersion
    }

    val sbtVersionComboBox = applyTo(new SComboBox[String]())(
      _.setItems(sbtVersions.versions.toArray),
      _.setSelectedItem(selections.sbtVersion)
    )

    val scalaVersionComboBox = applyTo(new SComboBox[String]())(setupScalaVersionItems)

    val zioVersionComboBox = applyTo(new SComboBox[String]())(setupZioVersionItems)

    val packagePrefixField = applyTo(new JBTextField())(
      _.setText(selections.packagePrefix.getOrElse("")),
      _.getEmptyText.setText(ScalaBundle.message("package.prefix.example"))
    )

    //noinspection TypeAnnotation
    val step = sdkSettingsStep(settingsStep)

    val resolveSbtClassifiersCheckBox = applyTo(new JCheckBox(SbtBundle.message("sbt.settings.sources")))(
      _.setToolTipText(SbtBundle.message("sbt.download.sbt.sources")),
      _.setSelected(selections.resolveSbtClassifiers)
    )

    val resolveClassifiersCheckBox: JCheckBox = applyTo(new JCheckBox(SbtBundle.message("sbt.settings.sources")))(
      _.setToolTipText(SbtBundle.message("sbt.download.scala.standard.library.sources")),
      _.setSelected(selections.resolveClassifiers)
    )

    val includeZioTestCheckBox: JCheckBox = applyTo(new JCheckBox("Include 'zio-test'"))(
      _.setToolTipText("Includes the ZIO Test library"),
      _.setSelected(selections.includeZioTest)
    )

    includeZioTestCheckBox.addActionListenerEx {
      selections.includeZioTest = includeZioTestCheckBox.isSelected
    }

    sbtVersionComboBox.addActionListenerEx {
      selections.sbtVersion = sbtVersionComboBox.getSelectedItem.asInstanceOf[String]
    }

    scalaVersionComboBox.addActionListenerEx {
      selections.scalaVersion = scalaVersionComboBox.getSelectedItem.asInstanceOf[String]

      val isScala3Selected     = isScala3Version(selections.scalaVersion)
      val supportedSbtVersions = if (isScala3Selected) sbtVersionsForScala3 else sbtVersions
      val supportedZioVersions =
        if (isScala3Selected) zioVersionsForScala3
        else ScalaVersion.fromString(selections.scalaVersion).map(loadZioVersions).getOrElse(zioVersions)
      sbtVersionComboBox.setItems(supportedSbtVersions.versions.toArray)
      zioVersionComboBox.setItems(supportedZioVersions.versions.toArray)
      // if we select Scala3 version but had Scala2 version selected before and some sbt version incompatible with Scala3,
      // the latest item from the list will be automatically selected
      sbtVersionComboBox.setSelectedItemSafe(selections.sbtVersion)
      sbtVersionComboBox.setSelectedItemSafe(selections.zioVersion)
      selections.update(SbtKind, sbtVersions)
    }
    zioVersionComboBox.addActionListenerEx {
      selections.zioVersion = zioVersionComboBox.getSelectedItem.asInstanceOf[String]
    }
    resolveClassifiersCheckBox.addActionListenerEx {
      selections.resolveClassifiers = resolveClassifiersCheckBox.isSelected
    }
    resolveSbtClassifiersCheckBox.addActionListenerEx {
      selections.resolveSbtClassifiers = resolveSbtClassifiersCheckBox.isSelected
    }
    packagePrefixField.getDocument.addDocumentListener(
      (_ => selections.packagePrefix = Option(packagePrefixField.getText).filter(_.nonEmpty)): DocumentAdapter
    )

    val sbtVersionPanel = applyTo(new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0)))(
      _.add(sbtVersionComboBox),
      _.add(resolveSbtClassifiersCheckBox)
    )

    val scalaVersionPanel = applyTo(new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0)))(
      _.setBorder(new border.EmptyBorder(1, 0, 0, 0)),
      _.add(scalaVersionComboBox),
      _.add(resolveClassifiersCheckBox)
    )

    val createHelloWorldCheckBox: JCheckBox = applyTo(new JCheckBox("""Create a "Hello World" main app"""))(
      _.setBorder(new border.EmptyBorder(0, 10, 0, 0)),
      _.setToolTipText("Adds a 'Main.scala' file using a zio.App that prints an output to the console."),
      _.setSelected(selections.includeHelloWorld)
    )
    createHelloWorldCheckBox.addActionListenerEx {
      selections.includeHelloWorld = createHelloWorldCheckBox.isSelected
    }

    val zioVersionPanel = applyTo(new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 0)))(
      _.setBorder(new border.EmptyBorder(1, 0, 0, 0)),
      _.add(zioVersionComboBox),
      _.add(includeZioTestCheckBox),
      _.add(createHelloWorldCheckBox)
    )

    settingsStep.addSettingsField(SbtBundle.message("sbt.settings.sbt"), sbtVersionPanel)
    settingsStep.addSettingsField(SbtBundle.message("sbt.settings.scala"), scalaVersionPanel)
    settingsStep.addSettingsField("ZIO:", zioVersionPanel)
    settingsStep.addSettingsField(
      ScalaBundle.message("package.prefix.label"),
      UI.PanelFactory.panel(packagePrefixField).withTooltip(ScalaBundle.message("package.prefix.help")).createPanel()
    )

    // TODO Remove the label patching when the External System will use the concise and proper labels natively
    Option(sbtVersionPanel.getParent).foreach { parent =>
      parent.getComponents.toSeq.foreachDefined {
        case label: JLabel if label.getText == "Project SDK:" =>
          label.setText("JDK:")
          label.setDisplayedMnemonic('J')
        case label: JLabel if label.getText == "ZIO:" =>
          label.setText("ZIO:")
          label.setDisplayedMnemonic('Z')

        case label: JLabel if label.getText.startsWith("Project ") && label.getText.length > 8 =>
          label.setText(label.getText.substring(8) |> (s => s.substring(0, 1).toUpperCase + s.substring(1)))
      }
    }

    step
  }

  private def isScala3Version(scalaVersion: String) = scalaVersion.startsWith("3")

  private def sdkSettingsStep(settingsStep: SettingsStep) = new SdkSettingsStep(
    settingsStep,
    this,
    (_: SdkTypeId).isInstanceOf[JavaSdk]
  ) {

    override def updateDataModel(): Unit =
      settingsStep.getContext.setProjectJdk(myJdkComboBox.getSelectedJdk)

    override def validate(): Boolean = super.validate() && {
      for {
        sdk     <- Option(myJdkComboBox.getSelectedJdk)
        version <- Option(selections.scalaVersion)

        languageLevel <- ScalaLanguageLevel.findByVersion(version)
      } validateLanguageLevel(languageLevel, sdk)

      true
    }

    private def validateLanguageLevel(languageLevel: ScalaLanguageLevel, sdk: Sdk): Unit = {
      import JavaSdkVersion.JDK_1_8
      import ScalaLanguageLevel._

      def reportMisconfiguration(libraryName: String, libraryVersion: String) =
        throw new ConfigurationException(
          SbtBundle
            .message("scala.version.requires.library.version", languageLevel.getVersion, libraryName, libraryVersion),
          SbtBundle.message("wrong.library.version", libraryName)
        )

      languageLevel match {
        case _ if languageLevel >= Scala_2_12 && !JavaSdk.getInstance().getVersion(sdk).isAtLeast(JDK_1_8) =>
          reportMisconfiguration("JDK", JDK_1_8.getDescription)
        case _ =>
      }
    }
  }

  private def setupScalaVersionItems(cbx: SComboBox[String]): Unit = {
    val versions = scalaVersions.versions
    cbx.setItems(versions.toArray)

    selections.scalaVersion match {
      case version if versions.contains(version) =>
        cbx.setSelectedItem(version)
      case _ if cbx.getItemCount > 0 => cbx.setSelectedIndex(0)
      case _                         =>
    }
  }

  private def setupZioVersionItems(cbx: SComboBox[String]): Unit = {
    val versions = zioVersions.versions
    cbx.setItems(versions.toArray)

    selections.zioVersion match {
      case version if versions.contains(version) =>
        cbx.setSelectedItem(version)
      case _ if cbx.getItemCount > 0 => cbx.setSelectedIndex(0)
      case _                         =>
    }
  }

  override def getNodeIcon: Icon = ZioIcon

  override def setupRootModel(model: ModifiableRootModel): Unit = SbtModuleBuilderUtil.tryToSetupRootModel(
    model,
    getContentEntryPath
  )

  // TODO customize the path in UI when IDEA-122951 will be implemented
  protected def updateModuleFilePath(pathname: String): String = {
    val file = new File(pathname)
    file.getParent + "/" + Sbt.ModulesDirectory + "/" + file.getName.toLowerCase
  }
}

object ZioProjectBuilder {

  import Sbt._
  import org.jetbrains.sbt._

  @NonNls private val Scala3RequiredSbtPlugins =
    """addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.4")
      |""".stripMargin

  private final case class Selections(
    var sbtVersion: String,
    var scalaVersion: String,
    var zioVersion: String,
    var sbtPlugins: String,
    var resolveClassifiers: Boolean,
    var resolveSbtClassifiers: Boolean,
    var includeZioTest: Boolean,
    var includeHelloWorld: Boolean,
    var packagePrefix: Option[String]
  ) {

    import Versions.{Kind, SBT => SbtKind, Scala => ScalaKind}

    def apply(kind: Kind): String = kind match {
      case ScalaKind => scalaVersion
      case SbtKind   => sbtVersion
    }

    def update(kind: Kind, versions: Versions): Unit = {
      val version = apply(kind) match {
        case null =>
          val Versions(defaultVersion, versionsArray) = versions
          versionsArray.headOption.getOrElse(defaultVersion)
        case value => value
      }

      kind match {
        case ScalaKind => scalaVersion = version
        case SbtKind   => sbtVersion = version
      }
    }
  }

  private def createProjectTemplateIn(
    root: File,
    @NonNls name: String,
    @NonNls scalaVersion: String,
    @NonNls sbtVersion: String,
    @NonNls zioVersion: String,
    @NonNls sbtPlugins: String,
    includeZioTest: Boolean,
    includeHelloWorld: Boolean,
    packagePrefix: Option[String]
  ): Unit = {
    val buildFile  = root / BuildFile
    val projectDir = root / ProjectDirectory

    if (buildFile.createNewFile() && projectDir.mkdir()) {
      (root / "src" / "main" / "scala").mkdirs()
      (root / "src" / "test" / "scala").mkdirs()

      import io.FileUtil.writeToFile

      val dependencies = new StringBuilder
      dependencies.append(s""""dev.zio" %% "zio" % "$zioVersion"""")
      if (includeZioTest) {
        dependencies.append(",").append(System.lineSeparator)
        dependencies.append(s"""      "dev.zio" %% "zio-test" % "$zioVersion" % Test""")
      }

      val sbtRunner =
        if (includeZioTest)
          s""",
             |    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")""".stripMargin
        else ""

      writeToFile(
        buildFile,
        s"""ThisBuild / scalaVersion     := "$scalaVersion"
           |ThisBuild / version          := "0.1.0-SNAPSHOT"
           |ThisBuild / organization     := "${packagePrefix.fold("com.example")(identity)}"
           |ThisBuild / organizationName := "example"
           |
           |lazy val root = (project in file("."))
           |  .settings(
           |    name := "$name",
           |    libraryDependencies ++= Seq(
           |      ${dependencies.result}
           |    )$sbtRunner
           |  )
           |""".stripMargin
      )
      writeToFile(
        projectDir / PropertiesFile,
        "sbt.version = " + sbtVersion
      )
      if (includeHelloWorld)
        writeToFile(
          root / "src" / "main" / "scala" / "Main.scala",
          scalaVersion match {
            case s if s.startsWith("3") =>
              s"""import zio._
                 |import zio.console.putStrLn
                 |
                 |object Main extends App:
                 |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
                 |    putStrLn("Welcome to your first ZIO app!").exitCode""".stripMargin
            case _ =>
              s"""import zio._
                 |import zio.console.putStrLn
                 |
                 |object Main extends App {
                 |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
                 |    putStrLn("Welcome to your first ZIO app!").exitCode
                 |}""".stripMargin
          }
        )

      import text.StringUtil.isEmpty
      if (!isEmpty(sbtPlugins)) {
        writeToFile(
          projectDir / PluginsFile,
          sbtPlugins
        )
      }
    }
  }
}
