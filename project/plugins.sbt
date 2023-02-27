addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "3.18.3")
addSbtPlugin("org.scalameta" % "sbt-scalafmt"    % "2.4.6")
addSbtPlugin("dev.zio"       % "zio-sbt-website" % "0.3.10+40-28fcc4a7-SNAPSHOT")

resolvers ++= Resolver.sonatypeOssRepos("public")
