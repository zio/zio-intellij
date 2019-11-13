import sbt.Keys.{onLoadMessage, version}

object BuildHelper {
  lazy val ScalacOptions = Seq(
    "-encoding",
    "UTF-8",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-Xlint:nullary-unit",
    "-Xlint:nullary-override",
    "-Xlint:inaccessible",
    "-Xlint:missing-interpolator",
    "-Xlint:doc-detached",
    "-Xlint:private-shadow",
    "-Xlint:type-parameter-shadow",
    "-Xlint:delayedinit-select",
    "-Xlint:option-implicit",
    "-Xlint:poly-implicit-overload",
    "-Xlint:infer-any",
    "-Ywarn-extra-implicit",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:privates",
    "-Ywarn-unused:implicits"
  )

  def welcomeMessage = onLoadMessage := {
    import scala.Console

    def header(text: String): String = s"${Console.RED}$text${Console.RESET}"

    def item(text: String): String = s"${Console.GREEN}▶ ${Console.CYAN}$text${Console.RESET}"

    s"""|${header(s"ZIO IntelliJ ${version.value}")}
        |Useful sbt tasks:
        |${item("fmt")} - Formats source files using scalafmt
        |${item("runIDE")} - runs IntelliJ with this plugin""".stripMargin
  }
}
