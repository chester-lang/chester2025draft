addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "3.26.2")

// cli tools
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.0+9-d2c99b38-SNAPSHOT")
resolvers ++= Resolver.sonatypeOssRepos("snapshots")
