import org.jetbrains.sbtidea.Keys._

addCommandAlias("format", "scalafmtAll ; scalafmtSbt ; scalafixAll")
addCommandAlias("fmt", "scalafmtAll ; scalafmtSbt")
inThisBuild(
  List(
    semanticdbEnabled := true, // enable SemanticDB
    semanticdbVersion := scalafixSemanticdb.revision // only required for Scala 2.x
  )
)
lazy val chesterPlugin =
  project
    .in(file("."))
    .enablePlugins(SbtIdeaPlugin)
    .settings(
      name := "ChesterLanguageSupport",
      version := "0.0.23",
      scalaVersion := "3.6.1",
      ThisBuild / intellijPluginName := "Chester Language Support",
      ThisBuild / intellijBuild := "242.23339.11",
      ThisBuild / intellijPlatform := IntelliJPlatform.IdeaCommunity,
      Global / intellijAttachSources := true,
      Compile / javacOptions ++= Seq("--release", "17"),
      intellijPlugins ++= Seq(
        "com.intellij.properties".toPlugin,
        "com.redhat.devtools.lsp4ij".toPlugin
      ),
      resolvers += "jitpack" at "https://jitpack.io",
      scalacOptions ++= Seq(
        //"-experimental"
      ),
      // scalafix
      scalacOptions ++= Seq("-Wunused:all", "-Xlint:adapted-args"),
      // Exclude LSP4J dependencies
      libraryDependencies ++= Seq(
        ("com.github.chester-lang.chester" %% "lsp" % "0.0.23")
          .exclude("org.eclipse.lsp4j", "org.eclipse.lsp4j")
          .exclude("org.eclipse.lsp4j", "org.eclipse.lsp4j.jsonrpc"),
        "com.eclipsesource.minimal-json" % "minimal-json" % "0.9.5"
      ),
      Compile / unmanagedResourceDirectories += baseDirectory.value / "resources",
      Test / unmanagedResourceDirectories += baseDirectory.value / "testResources",
      intellijVMOptions ~= { options =>
        options.add("--add-opens=java.desktop/javax.swing.text=ALL-UNNAMED")
      },
      Compile / resourceGenerators += Def.task {
        val logoSource =
          baseDirectory.value / ".." / "resources" / "chester-logo.svg"
        val logoTarget =
          baseDirectory.value / "resources" / "META-INF" / "pluginIcon.svg"
        IO.copyFile(logoSource, logoTarget)
        Seq(logoTarget)
      }
    )
