addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "3.26.2")

// cli tools
resolvers ++= Resolver.sonatypeOssRepos("snapshots")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.4")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.0")
