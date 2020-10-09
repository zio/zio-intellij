lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.2.1.2" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold("")("." + _)

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "202"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

lazy val `zio-intellij` = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    scalaVersion := scala212,
    version := pluginVersion,
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala:2020.2.27".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        Welcome to another exciting release of the ZIO IntelliJ Plugin! Huge thanks to all the contributors who made this release possible!<br/>
        Here are the highlights:
        <ul>
        <li>'Find usages' feature for macro created classes with <code>@accessible</code> (<a href="https://github.com/zio/zio-intellij/pull/152">#152</a>)</li>
        <li>Fixed ZIO Test suggestions erroneously appearing in other frameworks, such as specs2 (<a href="https://github.com/zio/zio-intellij/pull/158">#158</a>)</li>
        <li>Preserving the ZIO alias used in expression with static member reference (<a href="https://github.com/zio/zio-intellij/pull/161">#161</a>)</li>
        <li>Supporting running test methods/suites outside of the test class (i.e. in an object) (<a href="https://github.com/zio/zio-intellij/pull/162">#162</a>)</li>
        <li>Adding simplifications for <code>ZIO.fail</code> usages (<a href="https://github.com/zio/zio-intellij/pull/168">#168</a>)</li>
        <li>Add simplification from <code>.tap</code> to <code>.zipLeft</code> (<a href="https://github.com/zio/zio-intellij/pull/169">#169</a>)</li>
        <li>Miscellaneous tweaks and improvements</li>
        <ul>
        ]]>"""
      )
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
