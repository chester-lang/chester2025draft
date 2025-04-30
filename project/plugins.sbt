//dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.2.0" // scalablytyped & sbt-microsites
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.19.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.7")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.21.1+36-2d9cbce2-SNAPSHOT")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta44")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.1")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.4")
//addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")
addSbtPlugin("com.eed3si9n.ifdef" % "sbt-ifdef" % "0.4.1")
//addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.3.1")
//addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.4.4")
//addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.1.1")
//addSbtPlugin("com.github.sbt" % "sbt-proguard" % "0.5.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
//addSbtPlugin("io.get-coursier" % "sbt-shading" % "2.1.5")

dependencyUpdatesFilter -= moduleFilter(organization = "org.scala-lang")

// cli tools
resolvers ++= Resolver.sonatypeOssRepos("snapshots")
addDependencyTreePlugin
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.4")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.2+17-4ba873d2-SNAPSHOT")
//addSbtPlugin("com.github.sbt" % "sbt-license-report" % "1.7.0")
