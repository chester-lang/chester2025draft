//dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.2.0" // scalablytyped & sbt-microsites
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.17.0")
val nativeVer = if (System.getProperty("os.name").toLowerCase.contains("win")) "0.5.5" else "0.5.6"
addSbtPlugin("org.scala-native" % "sbt-scala-native" % nativeVer)
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta44")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.3.0")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.4")
//addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")
addSbtPlugin("com.eed3si9n.ifdef" % "sbt-ifdef" % "0.3.0")
//addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.3.1")
//addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.4.4")
//addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.1.1")
//addSbtPlugin("com.github.sbt" % "sbt-proguard" % "0.5.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
//addSbtPlugin("io.get-coursier" % "sbt-shading" % "2.1.5")

libraryDependencies ++= Seq(
  "org.mozilla" % "rhino" % "1.8.0",
  "org.mozilla" % "rhino-tools" % "1.8.0"
)

// cli tools
resolvers ++= Resolver.sonatypeOssRepos("snapshots")
addDependencyTreePlugin
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.0+27-2e43b9bf-SNAPSHOT")
//addSbtPlugin("com.github.sbt" % "sbt-license-report" % "1.7.0")
