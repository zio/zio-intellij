package zio.intellij.project.npw.template.wizard

import com.intellij.openapi.module.{ModifiableModuleModel, Module}
import com.intellij.openapi.util.io
import org.jetbrains.annotations.NonNls
import org.jetbrains.plugins.scala.ScalaVersion
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.project.Versions
import org.jetbrains.sbt.Sbt
import org.jetbrains.sbt.project.template.SbtModuleBuilderBase
import zio.intellij.ZioIcon
import zio.intellij.utils.ZioVersion.ZIO

import java.io.File
import javax.swing._
import scala.collection.mutable

final class ZioProjectBuilder(_selections: ZioProjectBuilder.Selections) extends SbtModuleBuilderBase {

  import ZioProjectBuilder._

  private val selections = _selections.copy() // Selections is mutable data structure

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
      val zioVersion   = zioVersionOpt.getOrElse(ZIO.`2.x.latest`.toString)

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

  override def getNodeIcon: Icon = ZioIcon
}

object ZioProjectBuilder {

  import Sbt._

  final case class Selections(
    var sbtVersion: Option[String],
    var scalaVersion: Option[String],
    var zioVersion: Option[String],
    var downloadScalaSdkSources: Boolean,
    var downloadSbtSources: Boolean,
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

      val dependencies = new mutable.StringBuilder
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
           |ThisBuild / organization     := "${packagePrefix.getOrElse("com.example")}"
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
  def apply(scalaVersion: String, zioVersion: String): String =
    (scalaVersion, zioVersion) match {
      case (s, z) if s.startsWith("3") && z.startsWith("1") =>
        s"""|import zio.*
            |import zio.console.putStrLn
            |
            |object Main extends App:
            |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
            |    putStrLn("Welcome to your first ZIO app!").exitCode""".stripMargin

      case (s, z) if s.startsWith("3") && z.startsWith("2") =>
        s"""|import zio.*
            |import zio.Console.printLine
            |
            |object Main extends ZIOAppDefault:
            |  override def run: ZIO[Environment & ZIOAppArgs & Scope, Any, Any] =
            |    printLine("Welcome to your first ZIO app!")""".stripMargin

      case (s, z) if s.startsWith("2") && z.startsWith("1") =>
        s"""|import zio._
            |import zio.console.putStrLn
            |
            |object Main extends App {
            |  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
            |    putStrLn("Welcome to your first ZIO app!").exitCode
            |}""".stripMargin

      case _ => // scala 2, zio 2
        s"""|import zio._
            |import zio.Console.printLine
            |
            |object Main extends ZIOAppDefault {
            |  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] =
            |    printLine("Welcome to your first ZIO app!")
            |}""".stripMargin
    }
}
