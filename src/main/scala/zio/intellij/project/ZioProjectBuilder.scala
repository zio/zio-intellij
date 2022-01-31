package zio.intellij.project

import com.intellij.ide.util.projectWizard.{ModuleWizardStep, SettingsStep}
import com.intellij.openapi.module.{JavaModuleType, ModifiableModuleModel, Module, ModuleType}
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.projectRoots.{JavaSdk, JavaSdkVersion, Sdk}
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.util.io
import com.intellij.ui.DocumentAdapter
import com.intellij.ui.components.JBTextField
import com.intellij.util.ui.UI
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.extensions.JComponentExt.ActionListenersOwner
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.project.template.ScalaVersionDownloadingDialog
import org.jetbrains.plugins.scala.project.{ScalaLanguageLevel, Version, Versions}
import org.jetbrains.plugins.scala.{extensions, ScalaBundle, ScalaVersion}
import org.jetbrains.sbt.project.template.SbtModuleBuilderUtil.doSetupModule
import org.jetbrains.sbt.project.template.{SComboBox, SbtModuleBuilderBase, SbtModuleBuilderUtil, ScalaSettingsStepBase}
import org.jetbrains.sbt.{Sbt, SbtBundle}
import zio.intellij.ZioIcon
import zio.intellij.utils.Version.ZIO
import zio.intellij.utils.{ScalaVersionHack, Version => ZioVersion}

import java.awt.FlowLayout
import java.io.File
import javax.swing._

private[zio] class ZioProjectBuilder extends SbtModuleBuilderBase {

  import Versions.{SBT => SbtKind, Scala => ScalaKind}
  import ZioProjectBuilder._

  private val selections = Selections(
    sbtVersion = None,
    scalaVersion = None,
    zioVersion = None,
    resolveClassifiers = true,
    resolveSbtClassifiers = false,
    includeZioTest = true,
    includeHelloWorld = true,
    packagePrefix = None
  )

  val hardcodedZioVersions = Versions(ZIO.`latest-ish`.toString, List("1.0.11", "1.0.10", "1.0.9"))

  private lazy val scalaVersions = ScalaKind.loadVersionsWithProgress()
  private lazy val sbtVersions   = SbtKind.loadVersionsWithProgress()
  private lazy val zioVersions = (for {
    versionStr <- selections.scalaVersion
    version    <- ScalaVersion.fromString(versionStr)
    results     = loadZioVersions(version)
  } yield results).getOrElse(hardcodedZioVersions)

  // Scala3 is only supported since sbt 1.5.0
  private val minSbtVersionForScala3 = "1.5.0"
  private lazy val sbtVersionsForScala3 = Versions(
    "1.5.5",
    sbtVersions.versions.filter(_ >= minSbtVersionForScala3)
  )

  private val minZioVersionForScala3 = ZIO.`1.0.8`
  private lazy val zioVersionsForScala3 = Versions(
    ZIO.`latest-ish`.toString,
    zioVersions.versions
      .map(ZioVersion.parseUnsafe)
      .filter(_ >= minZioVersionForScala3)
      .map(_.toString)
      .toList
  )
  private def loadZioVersions(scalaVersion: ScalaVersion) = {
    val versionPattern = ".+>(\\d+\\.\\d+\\.\\d+(?:-\\w+)?)/<.*".r

    def extractVersions(values: Seq[String]) =
      values.collect {
        case versionPattern(number) => number
      }

    val versionStr = if (isScala3Version(scalaVersion.versionStr)) "3" else scalaVersion.versionStr

    def loadVersions = {
      val url   = s"https://repo1.maven.org/maven2/dev/zio/zio_$versionStr/"
      val lines = Versions.loadLinesFrom(url)
      val versionStrings = lines.fold(
        Function.const(hardcodedZioVersions.versions),
        extractVersions
      )
      versionStrings.map(Version(_))
    }

    val versions = extensions
      .withProgressSynchronously("Fetching available ZIO versions")(loadVersions)
      .sorted
      .reverseIterator
      .map(_.presentation)
      .toList

    Versions(versions.headOption.getOrElse(hardcodedZioVersions.defaultVersion), versions)
  }

  override def createModule(moduleModel: ModifiableModuleModel): Module = {
    val root = new File(getModuleFileDirectory)
    if (root.exists()) {
      val Selections(
        sbtVersionOpt,
        scalaVersionOpt,
        zioVersionOpt,
        resolveClassifiers,
        resolveSbtClassifiers,
        includeZioTest,
        includeHelloWorld,
        packagePrefix
      )                = selections
      val sbtVersion   = sbtVersionOpt.getOrElse(Versions.SBT.LatestSbtVersion)
      val scalaVersion = scalaVersionOpt.getOrElse(ScalaVersion.Latest.Scala_2_13.minor)
      val zioVersion   = zioVersionOpt.getOrElse(ZIO.`latest-ish`.toString)

      locally {
        val settings = getExternalProjectSettings
        settings.setResolveClassifiers(resolveClassifiers)
        settings.setResolveSbtClassifiers(resolveSbtClassifiers)
      }

      doCreateProjectTemplateIn(
        root,
        getName,
        scalaVersion,
        sbtVersion,
        zioVersion,
        includeZioTest,
        includeHelloWorld,
        packagePrefix
      )

      setModuleFilePath(moduleFilePathUpdated(getModuleFilePath))
    }

    super.createModule(moduleModel)
  }

  override def modifySettingsStep(settingsStep: SettingsStep): ModuleWizardStep =
    new ZioWizardStep(settingsStep)

  final class ZioWizardStep(settingsStep: SettingsStep) extends ScalaSettingsStepBase(settingsStep, this) {

    locally {
      selections.update(ScalaKind, scalaVersions)
      selections.update(SbtKind, sbtVersions)
      selections.zioVersion = selections.zioVersion.orElse(zioVersions.versions.headOption)
    }

    private val sbtVersionComboBox = applyTo(new SComboBox[String]())(
      _.setItems(sbtVersions.versions.toArray),
      _.setSelectedItemSafe(selections.sbtVersion.orNull)
    )

    private val scalaVersionComboBox = applyTo(new SComboBox[String]())(
      setupScalaVersionItems
    )

    private val zioVersionComboBox = applyTo(new SComboBox[String]())(
      setupZioVersionItems
    )

    private val packagePrefixField = applyTo(new JBTextField())(
      _.setText(selections.packagePrefix.getOrElse("")),
      _.getEmptyText.setText(ScalaBundle.message("package.prefix.example"))
    )

    private val resolveClassifiersCheckBox: JCheckBox =
      applyTo(new JCheckBox(SbtBundle.message("sbt.settings.resolveClassifiers")))(
        _.setToolTipText(SbtBundle.message("sbt.download.scala.standard.library.sources")),
        _.setSelected(selections.resolveClassifiers)
      )

    private val resolveSbtClassifiersCheckBox =
      applyTo(new JCheckBox(SbtBundle.message("sbt.settings.resolveSbtClassifiers")))(
        _.setToolTipText(SbtBundle.message("sbt.download.sbt.sources")),
        _.setSelected(selections.resolveSbtClassifiers)
      )

    private val includeZioTestCheckBox: JCheckBox = applyTo(new JCheckBox("Include 'zio-test'"))(
      _.setToolTipText("Includes the ZIO Test library"),
      _.setSelected(selections.includeZioTest)
    )

    includeZioTestCheckBox.addActionListenerEx {
      selections.includeZioTest = includeZioTestCheckBox.isSelected
    }

    sbtVersionComboBox.addActionListenerEx {
      selections.sbtVersion = Option(sbtVersionComboBox.getSelectedItem.asInstanceOf[String])
    }

    scalaVersionComboBox.addActionListenerEx {
      selections.scalaVersion = Option(scalaVersionComboBox.getSelectedItem.asInstanceOf[String])

      val isScala3Selected     = selections.scalaVersion.exists(isScala3Version)
      val supportedSbtVersions = if (isScala3Selected) sbtVersionsForScala3 else sbtVersions
      val supportedZioVersions = if (isScala3Selected) zioVersionsForScala3 else zioVersions
      sbtVersionComboBox.setItems(supportedSbtVersions.versions.toArray)
      zioVersionComboBox.setItems(supportedZioVersions.versions.toArray)
      // if we select Scala3 version but had Scala2 version selected before and some sbt version incompatible with Scala3,
      // the latest item from the list will be automatically selected
      sbtVersionComboBox.setSelectedItemSafe(selections.sbtVersion.orNull)
      zioVersionComboBox.setSelectedItemSafe(selections.zioVersion.orNull)
      selections.update(SbtKind, sbtVersions)
    }
    zioVersionComboBox.addActionListenerEx {
      selections.zioVersion = Option(zioVersionComboBox.getSelectedItem.asInstanceOf[String])
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

    override def updateDataModel(): Unit =
      settingsStep.getContext.setProjectJdk(myJdkComboBox.getSelectedJdk)

    @throws[ConfigurationException]
    override def validate(): Boolean = super.validate() && {
      for {
        sdk     <- Option(myJdkComboBox.getSelectedJdk)
        version <- selections.scalaVersion

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

  private def isScala3Version(scalaVersion: String) = scalaVersion.startsWith("3")

  private def setupScalaVersionItems(cbx: SComboBox[String]): Unit = {
    val versions = scalaVersions.versions
    cbx.setItems(versions.toArray)

    selections.scalaVersion match {
      case Some(version) if versions.contains(version) =>
        cbx.setSelectedItemSafe(version)
        if (selections.scrollScalaVersionDialogToTheTop) {
          ScalaVersionDownloadingDialog.UiUtils.scrollToTheTop(cbx)
        }
      case _ if cbx.getItemCount > 0 =>
        cbx.setSelectedIndex(0)
      case _ =>
    }
  }

  private def setupZioVersionItems(cbx: SComboBox[String]): Unit = {
    val versions = zioVersions.versions
    cbx.setItems(versions.toArray)

    selections.zioVersion match {
      case Some(version) if versions.contains(version) =>
        cbx.setSelectedItemSafe(version)
        if (selections.scrollScalaVersionDialogToTheTop) {
          ScalaVersionDownloadingDialog.UiUtils.scrollToTheTop(cbx)
        }
      case _ if cbx.getItemCount > 0 =>
        cbx.setSelectedIndex(0)
      case _ =>
    }
  }

  override def getNodeIcon: Icon = ZioIcon
}

object ZioProjectBuilder {

  import Sbt._
  import org.jetbrains.sbt._

  private final case class Selections(
    var sbtVersion: Option[String],
    var scalaVersion: Option[String],
    var zioVersion: Option[String],
    var resolveClassifiers: Boolean,
    var resolveSbtClassifiers: Boolean,
    var includeZioTest: Boolean,
    var includeHelloWorld: Boolean,
    var packagePrefix: Option[String]
  ) {

    var scrollScalaVersionDialogToTheTop = false

    import Versions.{Kind, SBT => SbtKind, Scala => ScalaKind}

    def versionFromKind(kind: Kind): Option[String] = kind match {
      case ScalaKind => scalaVersion
      case SbtKind   => sbtVersion
    }

    def update(kind: Kind, versions: Versions): Unit = {
      val explicitlySelectedVersion = versionFromKind(kind)
      val version                   = explicitlySelectedVersion.getOrElse(kind.initiallySelectedVersion(versions.versions))

      kind match {
        case ScalaKind =>
          scalaVersion = Some(version)
          scrollScalaVersionDialogToTheTop = explicitlySelectedVersion.isEmpty
        case SbtKind =>
          sbtVersion = Some(version)
      }
    }
  }

  private def doCreateProjectTemplateIn(
    root: File,
    @NonNls name: String,
    @NonNls scalaVersion: String,
    @NonNls sbtVersion: String,
    @NonNls zioVersion: String,
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
           |      ${dependencies.result()}
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
          HelloWorld(scalaVersion, zioVersion)
        )
    }
  }
}
object HelloWorld {
  def apply(scalaVersion: String, zioVersion: String): String = {
    val (imp, prn) = versionSpecific(zioVersion)

    scalaVersion match {
      case s if s.startsWith("3") =>
        s"""import zio.*
           |import $imp
           |
           |object Main extends App:
           |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
           |    $prn("Welcome to your first ZIO app!").exitCode""".stripMargin
      case _ =>
        s"""import zio._
           |import $imp
           |
           |object Main extends App {
           |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
           |    $prn("Welcome to your first ZIO app!").exitCode
           |}""".stripMargin
    }
  }

  private def versionSpecific(zioVersion: String) =
    if (zioVersion.startsWith("2")) ("zio.Console.printLine", "printLine")
    else ("zio.console.putStrLn", "putStrLn")
}
