lazy val scala213      = "2.13.2"
lazy val pluginVersion = "2020.3.2.1" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold("")("." + _)

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "203.5600.34"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

scalacOptions += "-deprecation"

lazy val `zio-intellij` = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    scalaVersion := scala213,
    version := pluginVersion,
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala:2020.3.12".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        Welcome to another exciting release of the ZIO IntelliJ Plugin!<br/>
        This is the initial release supporting IntelliJ 2020.3. Please report any issues you might find!</br>
        </br>
        I'd like to thank Nikita Myazin, Timur Aliberdov, and others for contributing tons of features and bugfixes to this release!<br/>
        Here are the highlights:<br/>
        <ul>
        <li>Simplification from <code>layer.build.use</code> to <code>zio.provideLayer</code> (<a href="https://github.com/zio/zio-intellij/pull/172">#172</a>)</li>
        <li>Improved test method detection (<a href="https://github.com/zio/zio-intellij/pull/177">#177</a>)</li>
        <li>Detecting of <code>CanFail</code> modes to prevent erroneously handling errors on unfailing effects (<a href="https://github.com/zio/zio-intellij/pull/178">#178</a>, <a href="https://github.com/zio/zio-intellij/pull/180">#180</a>)</li>
        <li>Detecting of <code>NeedEnv</code> modes to prevent erroneously providing requirements to effects that don't require anything (<a href="https://github.com/zio/zio-intellij/pull/189">#189</a>)</li>
        <li>New refactoring: Suggest type alias (<a href="https://github.com/zio/zio-intellij/pull/183">#183</a>)</li>
        <li>Integrated ZIO Test runner support (experimental) (<a href="https://github.com/zio/zio-intellij/pull/179">#179</a>)</li>
        <li>Many many bug fixes and small improvements</li>
        <ul>
        ]]>"""
      )
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
