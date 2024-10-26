//dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.2.0" // scalablytyped & sbt-microsites
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.17.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.5")
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta44")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.2.0")
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.4")
//addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")
addSbtPlugin("com.eed3si9n.ifdef" % "sbt-ifdef" % "0.3.0")
//addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.3.1")
//addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.4.4")
//addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")
//addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.1.1")
//addSbtPlugin("com.github.sbt" % "sbt-proguard" % "0.5.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.12.0")
//addSbtPlugin("io.get-coursier" % "sbt-shading" % "2.1.5")

libraryDependencies ++= Seq(
  "org.mozilla" % "rhino" % "1.7.15"
)

// cli tools
resolvers ++= Resolver.sonatypeOssRepos("snapshots")
addDependencyTreePlugin
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.3")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.13.0+9-d2c99b38-SNAPSHOT")
addSbtPlugin("com.github.sbt" % "sbt-license-report" % "1.6.1")
libraryDependencies ++= Seq("com.google.googlejavaformat" % "google-java-format" % "1.24.0")
addSbtPlugin("com.lightbend.sbt" % "sbt-java-formatter" % "0.8.0")