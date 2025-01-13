// reads env: NATIVE_IMAGE_OPTIONS, VERSION
import org.scalajs.linker.interface.OutputPatterns
import sbt.librarymanagement.InclExclRule

import scala.scalanative.build.*
import sbt.complete.DefaultParsers._

import scala.sys.process._

import sbt.dsl.LinterLevel.Ignore

ThisBuild / version := sys.env.getOrElse("VERSION", "0.0.26")
ThisBuild / organization := "com.github.chester-lang"

addCommandAlias("testAll", "rootJVM/test ; rootJS/test ; rootNative/test")

addCommandAlias("format0", "scalafmtAll ; scalafmtSbt ; rootJVM/scalafixAll")
addCommandAlias("format", "scalafmtAll ; scalafmtSbt ; scalafixAll")
addCommandAlias("fmt", "scalafmtAll ; scalafmtSbt")
inThisBuild(
  List(
    semanticdbEnabled := true, // enable SemanticDB
    semanticdbVersion := scalafixSemanticdb.revision // only required for Scala 2.x
  )
)

lazy val up = inputKey[Unit]("Run pnpm install and update in site and vscode folders")
up := {
  val log = streams.value.log
  val folders = Seq(
    file("site"),
    file("vscode"),
    file("js-for-jvm")
  )

  folders.foreach { dir =>
    if (dir.exists()) {
      log.info(s"Updating dependencies in ${dir.getName}...")
      Process("pnpm install", dir) ! log
      Process("pnpm update", dir) ! log
    } else {
      log.warn(s"Directory ${dir.getName} does not exist, skipping")
    }
  }

  log.success("Finished updating all dependencies")
}

val scala3Version = "3.6.3-RC2"
val scala3Lib = "3.6.2"
val scala2Version = "2.13.16"

val graalVm = "graalvm-java23"
val graalJdkVersion = "23.0.1"
val graalvmVersion = "24.1.1"

val defaultNativeImageOptions = Seq(
  //"-H:-CheckToolchain",
  "--verbose",
  "--no-fallback",
  "-enablesystemassertions",
  // runtime: org.jline
  "--initialize-at-build-time=org.mozilla.javascript,org.slf4j,org.typelevel,os,scalax,sbt,ujson,upack,upickle,algebra,cps,com.oracle,spire,org.graalvm,scopt,fastparse,scala,java,chester,org.eclipse,cats,fansi,sourcecode,com.monovore.decline,geny,pprint",
  "--initialize-at-build-time=scala.meta.internal.semanticdb.Access$$anon$1",
  "-O2",
  // "-Dpolyglotimpl.DisableVersionChecks=true", // for 24-ea
  "-H:+AddAllCharsets" // https://stackoverflow.com/questions/74525670/graalvm-native-with-kotlin-unsupportedcharsetexception-cp1252/74528833#74528833
)

val classVersion =
  java.lang.Float.parseFloat(System.getProperty("java.class.version"))
val jdk17ClassVersion = 61.0f
val jdk17: Boolean = false /* because of -java-output-version 8 */
// classVersion >= jdk17ClassVersion

val commonSettings0 = Seq(
  // githubTokenSource := TokenSource.GitConfig("github.token") || TokenSource.Environment("GITHUB_TOKEN"),
  // resolvers += Resolver.githubPackages("edadma", "readline"),
  resolvers += "jitpack" at "https://jitpack.io",
  resolvers += Resolver.mavenLocal,
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  // some options can be found at https://github.com/typelevel/sbt-tpolecat/commit/d4dd41451a9e9346cf8c0253018bc648f6527be3
  scalacOptions ++=
    Seq(
      "-Wsafe-init",
      "-encoding",
      "utf8",
      "-explain-cyclic",
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-deprecation",
      "-feature"
      // "-experimental"
    ),
  // scalafix
  scalacOptions ++= Seq("-Wunused:all", "-Xlint:adapted-args"),
  scalacOptions ++= Seq("-rewrite", "-source", "3.6-migration"),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.0.4" % Test,
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    "org.scalatest" %%% "scalatest-funsuite" % "3.2.19" % Test,
    "org.scalatest" %%% "scalatest-shouldmatchers" % "3.2.19" % Test,
    "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test,
    "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test,
    "com.lihaoyi" %%% "pprint" % "0.9.0" % Test
    // "com.eed3si9n.verify" %%% "verify" % "1.0.0" % Test
  ),
  testFrameworks += new TestFramework("verify.runner.Framework"),
  excludeDependencies ++= Seq(
    ExclusionRule("com.lihaoyi", "fastparse_2.13"),
    ExclusionRule("com.lihaoyi", "fastparse_sjs1_2.13"),
    ExclusionRule("com.lihaoyi", "fastparse_native0.5_2.13"),
    ExclusionRule("com.lihaoyi", "sourcecode_2.13"),
    ExclusionRule("com.lihaoyi", "sourcecode_sjs1_2.13"),
    ExclusionRule("com.lihaoyi", "sourcecode_native0.5_2.13"),
    ExclusionRule("com.lihaoyi", "geny_2.13"),
    ExclusionRule("com.lihaoyi", "geny_sjs1_2.13"),
    ExclusionRule("com.lihaoyi", "geny_native0.5_2.13"),
    ExclusionRule("org.scala-native", "junit-runtime_native0.5_2.13"),
    ExclusionRule("org.scala-native", "test-interface_native0.5_2.13")
  )
)
val commonSettings = commonSettings0 ++ Seq(
  scalaVersion := scala3Version
)
val commonLibSettings = commonSettings0 ++ Seq(
  scalaVersion := scala3Lib
)
val scala2Common = Seq(
  scalaVersion := scala2Version,
  resolvers += "jitpack" at "https://jitpack.io",
  resolvers += Resolver.mavenLocal,
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  scalacOptions ++= Seq(
    "-encoding",
    "utf8",
    "-Wunused:imports",
    "-Ytasty-reader"
  ),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.0.4" % Test cross (CrossVersion.for2_13Use3),
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test cross (CrossVersion.for2_13Use3),
    "org.scalatest" %%% "scalatest-funsuite" % "3.2.19" % Test cross (CrossVersion.for2_13Use3),
    "org.scalatest" %%% "scalatest-shouldmatchers" % "3.2.19" % Test cross (CrossVersion.for2_13Use3),
    "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test cross (CrossVersion.for2_13Use3),
    "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test cross (CrossVersion.for2_13Use3)
  ),
  excludeDependencies ++= Seq(
    ExclusionRule("com.lihaoyi", "fastparse_2.13"),
    ExclusionRule("com.lihaoyi", "fastparse_sjs1_2.13"),
    ExclusionRule("com.lihaoyi", "fastparse_native0.5_2.13"),
    ExclusionRule("com.lihaoyi", "sourcecode_2.13"),
    ExclusionRule("com.lihaoyi", "sourcecode_sjs1_2.13"),
    ExclusionRule("com.lihaoyi", "sourcecode_native0.5_2.13"),
    ExclusionRule("com.lihaoyi", "geny_2.13"),
    ExclusionRule("com.lihaoyi", "geny_sjs1_2.13"),
    ExclusionRule("com.lihaoyi", "geny_native0.5_2.13"),
    ExclusionRule("org.scala-native", "junit-runtime_native0.5_3"),
    ExclusionRule("org.scala-native", "test-interface_native0.5_3")
  )
)
val commonVendorSettings = Seq(
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  scalaVersion := scala3Lib,
  scalacOptions ++= Seq("-java-output-version", "11"),
  scalacOptions += "-nowarn"
)
val scala2VendorSettings = Seq(
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  scalaVersion := scala2Version,
  scalacOptions ++= Seq("-java-output-version", "11"),
  scalacOptions += "-nowarn"
)
val cpsSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin(
    "com.github.rssh" %% "dotty-cps-async-compiler-plugin" % "0.9.23"
  )
)
val commonJvmLibSettings = Seq(
  // scalacOptions ++= (if (jdk17) Seq("-Xmacro-settings:com.eed3si9n.ifdef.declare:jdk17") else Seq()),
  scalacOptions ++= Seq("-java-output-version", "11")
)

val NativeImageOptions = sys.env.get("NATIVE_IMAGE_OPTIONS").map(_.split(" ").toList).getOrElse(List[String]())

val graalvmSettings = Seq(
  nativeImageVersion := graalJdkVersion,
  nativeImageOptions ++= defaultNativeImageOptions,
  nativeImageOptions ++= NativeImageOptions,
  nativeImageJvm := graalVm
)

val baseDeps = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %%% "cats-core" % "2.12.0",
    "org.typelevel" %%% "cats-free" % "2.12.0",
    "com.lihaoyi" %%% "upickle" % "4.1.0",
    "com.lihaoyi" %%% "fansi" % "0.5.0",
    "com.lihaoyi" %%% "fastparse" % "3.1.1"
    // "com.lihaoyi" %%% "scalatags" % "0.13.1",
    // "com.github.rssh" %%% "dotty-cps-async" % "0.9.23",
    // "io.getkyo" %%% "kyo-prelude" % "0.12.2",
    // "io.getkyo" %%% "kyo-data" % "0.12.2",
    // "io.getkyo" %%% "kyo-tag" % "0.12.2",
  )
)

commonSettings

lazy val bump = inputKey[Unit]("Bump version in multiple files")
bump := {
  val args: Seq[String] = spaceDelimited("<new_version>").parsed
  if (args.length != 1) {
    println("Usage: bump <new_version>")
  } else {
    val newVersion = args(0)
    val oldVersion = (ThisBuild / version).value
    val filesToUpdate = Seq(
      file("build.sbt"),
      file("idea-plugin/build.sbt"),
      file("idea-plugin/resources/META-INF/plugin.xml"),
      file("vscode/package.json"),
      file("cli/package.json"),
      file("packages/base/package.json")
    )
    val filesToUpdateWithoutQuotes = Seq(
      file("idea-plugin/resources/META-INF/plugin.xml")
    )

    filesToUpdate.foreach { f =>
      val content = IO.read(f)
      val updated = content.replaceAllLiterally(s""""$oldVersion"""", s""""$newVersion"""")
      IO.write(f, updated)
    }

    filesToUpdateWithoutQuotes.foreach { f =>
      val content = IO.read(f)
      val updated = content.replaceAllLiterally(oldVersion, newVersion)
      IO.write(f, updated)
    }

    // Update the version in build.sbt
    val buildSbtContent = IO.read(file("build.sbt"))
    val updatedBuildSbt = buildSbtContent.replace(
      s"""ThisBuild / version := sys.env.getOrElse("VERSION", "$oldVersion")""",
      s"""ThisBuild / version := sys.env.getOrElse("VERSION", "$newVersion")"""
    )
    IO.write(file("build.sbt"), updatedBuildSbt)

    println(s"Version bumped from $oldVersion to $newVersion in all specified files.")
  }
}

lazy val bumpScala = inputKey[Unit]("Bump Scala version in multiple files")
bumpScala := {
  val args: Seq[String] = spaceDelimited("<new_scala_version>").parsed
  if (args.length != 1) {
    println("Usage: bumpScala <new_scala_version>")
  } else {
    val newScalaVersion = args(0)
    val oldScalaVersion = scalaVersion.value
    val filesToUpdate = Seq(
      file("build.sbt"),
      file(".github/workflows/build.yml"),
      file("docs/dev.sh"),
      file("idea-plugin/build.sbt"),
      file("site/package.json"),
      file("cli/package.json"),
      file("packages/base/package.json")
    )

    filesToUpdate.foreach { f =>
      val content = IO.read(f)
      val updated = content.replaceAllLiterally(oldScalaVersion, newScalaVersion)
      IO.write(f, updated)
    }

    // Update the Scala version in build.sbt
    val buildSbtContent = IO.read(file("build.sbt"))
    val updatedBuildSbt = buildSbtContent.replace(
      s"""val scala3Version = "$oldScalaVersion"""",
      s"""val scala3Version = "$newScalaVersion""""
    )
    IO.write(file("build.sbt"), updatedBuildSbt)

    println(s"Scala version bumped from $oldScalaVersion to $newScalaVersion in all specified files.")
  }
}

ThisBuild / assemblyMergeStrategy := {
  case PathList("META-INF", "versions", "9", "module-info.class") =>
    MergeStrategy.discard
  case PathList(
        "module-info.class" | "plugin.xml" | "plugin.properties" | ".options" | ".api_description"
      ) =>
    MergeStrategy.discard
  case PathList("META-INF", "eclipse.inf")                    => MergeStrategy.discard
  case PathList("META-INF", "groovy-release-info.properties") => MergeStrategy.discard
  // our overrides
  case PathList("scala", "meta", "internal", "tokenizers", xml) if xml.contains("XmlParser") || xml.contains("ScalaExprPositionParser") =>
    MergeStrategy.preferProject
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

val supportNativeBuildForTermux = true

ThisBuild / nativeConfig ~= ((System.getProperty("os.name").toLowerCase, System.getProperty("os.arch").toLowerCase) match {
  case (mac, _) if mac.contains("mac") => { // mac has some bugs with optimizations
    _.withGC(GC.commix)
  }
  /*
  [error] /usr/bin/ld: /tmp/lto-llvm-7d968c.o: relocation R_AARCH64_ADR_PREL_PG_HI21 against symbol `__stack_chk_guard@@GLIBC_2.17' which may bind externally can not be used when making a shared object; recompile with -fPIC
  [error] /usr/bin/ld: /tmp/lto-llvm-7d968c.o(.text.MutatorThreads_init+0x8): unresolvable R_AARCH64_ADR_PREL_PG_HI21 relocation against symbol `__stack_chk_guard@@GLIBC_2.17'
  [error] /usr/bin/ld: final link failed: bad value
  [error] clang++: error: linker command failed with exit code 1 (use -v to see invocation)
  [info] Total (36687 ms)
   */
  // Archlinux aarch64 Virtual Machine on Apple Silicon: LTO is broken too
  case (linux, "aarch64") if linux.contains("linux") => {
    _.withMode(Mode.releaseFast)
      .withGC(GC.commix)
  }
  case _ => {
    _.withLTO(LTO.thin)
      .withMode(Mode.releaseFast)
      .withGC(GC.commix)
  }
})

ThisBuild / nativeConfig ~= (if (supportNativeBuildForTermux) {
                               _.withMultithreading(false).withGC(GC.immix)
                             } else (x => x))

// original kiama-core
lazy val kiamaCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("vendor/kiama-core"))
  .settings(
    commonVendorSettings
  )
  .disablePlugins(ScalafixPlugin)
  .jvmSettings(commonJvmLibSettings)

// kiama fork from effekt - https://github.com/effekt-lang/kiama
lazy val effektKiama = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("vendor/effekt-kiama"))
  .settings(
    commonVendorSettings
  )
  .disablePlugins(ScalafixPlugin)
  .jvmSettings(
    commonJvmLibSettings,
    libraryDependencies ++= Seq(
      "jline" % "jline" % "2.14.6",
      "org.rogach" %% "scallop" % "5.2.0",
      "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.24.0-SNAPSHOT",
      "com.google.code.gson" % "gson" % "2.11.0"
    )
  )
  .nativeSettings(
    // https://github.com/scala-native/scala-native/issues/4044#issuecomment-2329088930
    scalacOptions ++= Seq(
      "-Ylegacy-lazy-vals"
    )
  )

// iron & iron-cats & iron-upickle, commit 86fbe48e8c9b0f6e5d2f7261ddefaa7c671341ae, built against Scala Native 0.5
// removed RefinedTypeOpsSuite.scala because of compilation error
lazy val ironNative = crossProject(NativePlatform)
  .withoutSuffixFor(NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("vendor/iron-native"))
  .settings(
    commonVendorSettings
  )
  .disablePlugins(ScalafixPlugin)
  .nativeSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "4.1.0",
      "org.typelevel" %%% "cats-core" % "2.12.0",
      "com.lihaoyi" %%% "utest" % "0.8.5" % Test,
      "org.typelevel" %%% "kittens" % "3.4.0" % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

// commit 52b3692bdfe01ef6c645380b02595a9c60a9725b, core & util & platform & macros, main only, no tests
// rewrite by scalac with 3.4-migration
// needed project/GenProductTypes.scala
lazy val genProductTypes = TaskKey[Seq[File]](
  "gen-product-types",
  "Generates several type classes for Tuple2-22."
)
lazy val spireNative = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(NativePlatform)
  .crossType(CrossType.Full)
  .in(file("vendor/spire-native"))
  .disablePlugins(ScalafixPlugin)
  .settings(
    scalacOptions ++= Seq("-rewrite", "-source", "3.4-migration"),
    commonVendorSettings,
    Compile / sourceGenerators += (Compile / genProductTypes).taskValue,
    genProductTypes := {
      val scalaSource = (Compile / sourceManaged).value
      val s = streams.value
      s.log.info("Generating spire/std/tuples.scala")
      val algebraSource = ProductTypes.algebraProductTypes
      val algebraFile = (scalaSource / "spire" / "std" / "tuples.scala").asFile
      IO.write(algebraFile, algebraSource)

      Seq[File](algebraFile)
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "algebra-laws" % "2.12.0"
    )
  )
  .nativeSettings(
  )
  .jvmSettings(commonJvmLibSettings)

// split modules trying to increase incremental compilation speed
lazy val utils = useSpire(
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("utils"))
    .settings(
      commonLibSettings,
      baseDeps,
      libraryDependencies ++= Seq(
        "org.scala-graph" %%% "graph-core" % "2.0.2"
      )
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "os-lib" % "0.11.4-M4"
      ),
      commonJvmLibSettings,
      libraryDependencies ++= Seq(
        "io.github.iltotore" %%% "iron" % "2.6.0",
        "io.github.iltotore" %%% "iron-cats" % "2.6.0",
        "io.github.iltotore" %%% "iron-upickle" % "2.6.0" exclude ("com.lihaoyi", "upickle_3")
      ),
      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-stubs" % "1.1.0"
      ),
      libraryDependencies ++= Seq(
        // "it.unimi.dsi" % "fastutil" % "8.5.14",
      ),
      libraryDependencies += "org.graalvm.sdk" % "nativeimage" % graalvmVersion
    )
    .nativeSettings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "os-lib" % "0.11.4-M4"
      ),
      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-stubs" % "1.1.0"
      )
    )
    .nativeConfigure(_.dependsOn(ironNative.native))
    .jsSettings(
      libraryDependencies ++= Seq(
        "io.github.iltotore" %%% "iron" % "2.6.0",
        "io.github.iltotore" %%% "iron-cats" % "2.6.0",
        "io.github.iltotore" %%% "iron-upickle" % "2.6.0" exclude ("com.lihaoyi", "upickle_3")
      )
    )
)

def useSpire(
    project: _root_.sbtcrossproject.CrossProject
): _root_.sbtcrossproject.CrossProject =
  project
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "spire" % "0.18.0"
      )
    )
    .nativeSettings(
      libraryDependencies ++= Seq(
        // "com.github.mio-19.spire" /*"org.typelevel"*/ %%% "spire" % "fcf7d67b61",
      )
    )
    .nativeConfigure(_.dependsOn(spireNative.native))
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "spire" % "0.18.0"
      )
    )

lazy val pretty = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("pretty"))
  .dependsOn(utils)
  .dependsOn(kiamaCore)
  .settings(
    commonLibSettings
  )
  .jvmSettings(commonJvmLibSettings)

lazy val reader = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("reader"))
  .dependsOn(utils, syntax)
  .settings(
    commonLibSettings
  )
  .jvmSettings(commonJvmLibSettings)

val dependOnGraal = Seq(
  libraryDependencies ++= Seq(
    "org.graalvm.truffle" % "truffle-api" % graalvmVersion,
    "org.graalvm.truffle" % "truffle-dsl-processor" % graalvmVersion,
    "org.graalvm.truffle" % "truffle-tck" % graalvmVersion,
    "org.graalvm.sdk" % "graal-sdk" % graalvmVersion
  )
)

lazy val syntax = useSpire(
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("syntax"))
    .dependsOn(utils, pretty)
    .settings(
      commonLibSettings
    )
    .jvmSettings(commonJvmLibSettings)
    .jvmSettings(dependOnGraal)
)

lazy val err = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("err"))
  .dependsOn(syntax)
  .settings(
    commonLibSettings
  )
  .jvmSettings(commonJvmLibSettings)

lazy val tyckBase = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("tyck-base"))
  .dependsOn(utils, syntax, err)
  .settings(
    name := "tyck-base",
    commonSettings
  )
  .jvmSettings(commonJvmLibSettings)

lazy val tyck = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("tyck"))
  .dependsOn(utils, syntax, err, tyckBase, eval)
  .settings(
    commonSettings
  )
  .jvmSettings(commonJvmLibSettings)

// compiles with scala native. XmlParser broken on nativeLink step. will wait for scalameta scala3 migration
lazy val compiler213 = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("compiler213"))
  .dependsOn(syntax, err)
  .settings(
    scala2Common,
    libraryDependencies += ("org.scalameta" %%% "scalameta" % "4.12.4.1")
      .cross(CrossVersion.for3Use2_13)
      .exclude("org.jline", "jline"),
    // scalap is a dependency of scalameta
    libraryDependencies += ("org.scala-lang" % "scalap" % scala2Version).exclude("org.jline", "jline")
  )
  .jvmSettings(commonJvmLibSettings)
lazy val compiler = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("compiler"))
  .dependsOn(utils, syntax, err)
  .jvmConfigure(_.dependsOn(compiler213.jvm))
  .jsConfigure(_.dependsOn(compiler213.js))
  .settings(
    name := "compiler",
    commonSettings
  )
  .jvmSettings(commonJvmLibSettings)

// jvm holds stub for class interface.
lazy val jsForJvm = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Full)
  .in(file("js-for-jvm"))
  .settings(
    commonSettings,
    name := "js-for-jvm"
  )
  .jsConfigure(_.dependsOn(utils.js))
  .jsSettings(
    scalaJSLinkerConfig ~= {
      // Enable ECMAScript module output.
      _.withModuleKind(ModuleKind.ESModule)
        // Use .mjs extension.
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    }
  )
  .jvmSettings(
    commonJvmLibSettings,
    libraryDependencies ++= Seq(
      "org.mozilla" % "rhino" % "1.7.15"
    )
  )

val sootupVersion = "1.3.0"
lazy val platform = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("platform"))
  .dependsOn(platform0, compiler)
  .settings(
    name := "platform",
    commonSettings
  )
  .jvmConfigure(_.dependsOn(jsForJvm.jvm % Provided))
  .jvmSettings(
    // Ensure that tyckPlatform.jvm depends on jsForJvm's fastLinkJS task
    Compile / compile := (Compile / compile)
      .dependsOn(jsForJvm.js / Compile / fastLinkJS)
      .value,
    /*
    // Modify the source generator to use Def.taskDyn
    Compile / sourceGenerators += Def.taskDyn {
      // Use Def.taskDyn to create a dynamic dependency on jsForJvm.js / fastLinkJS
      (jsForJvm.js / Compile / fastLinkJS).map { jsLinkerOutput =>
        val jsArtifact = (jsForJvm.js / Compile / fastLinkJSOutput).value / jsLinkerOutput.data.publicModules.head.jsFileName

        val log = streams.value.log

        // Copy to file("js-for-jvm") / "index.js"
        IO.copyFile(jsArtifact, file("js-for-jvm") / "index.js")
        Process("pnpm install", file("js-for-jvm")) ! log
        Process("pnpm run build", file("js-for-jvm")) ! log

        // Read the content of the JS file
        val jsContent = IO.read(file("js-for-jvm") / "dist" / "bundle.js")

        // Escape special characters in the JS content
        val escapedJsContent = jsContent
          .replace("\\", "\\\\") // Escape backslashes
          .replace("\"\"\"", "\\\"\\\"\\\"") // Escape triple quotes if any

        // Define where to place the generated Scala file
        val sourceDir = (Compile / sourceManaged).value
        val generatedFile = sourceDir / "chester" / "generated" / "GeneratedJS.scala"

        // Generate the content of the Scala file
        val content =
          s"""package chester.generated
object GeneratedJS {
  val jsCode: String = \"\"\"$escapedJsContent\"\"\"
}
          """

        // Write the content to the Scala file
        IO.write(generatedFile, content)

        // Return the generated file
        Seq(generatedFile)
      }
    }.taskValue,
     */
    // note that won't run on compile only package and run  - https://github.com/sbt/sbt/issues/1832
    Compile / resourceGenerators += Def.taskDyn {
      (jsForJvm.js / Compile / fastLinkJS).map { jsLinkerOutput =>
        val jsArtifact = (jsForJvm.js / Compile / fastLinkJSOutput).value / jsLinkerOutput.data.publicModules.head.jsFileName

        val log = streams.value.log

        // Copy to file("js-for-jvm") / "index.js"
        IO.copyFile(jsArtifact, file("js-for-jvm") / "index.js")
        Process("pnpm install", file("js-for-jvm")) ! log
        Process("pnpm run build", file("js-for-jvm")) ! log
        val jsFile = (file("js-for-jvm") / "dist" / "bundle.js").getAbsolutePath
        val dest = (Compile / resourceManaged).value

        org.mozilla.javascript.tools.jsc.Main
          .main(Array("-opt", "9", "-version", "200", "-nosource", "-d", dest.getAbsolutePath, "-package", "chester", "-o", "ChesterJs", jsFile))
        Seq(dest / "chester" / "ChesterJs.class")
      }
    }.taskValue,
    commonJvmLibSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scalap" % scala2Version exclude ("org.jline", "jline"), // dependency of semanticdb-shared
      "org.scalameta" %% "semanticdb-shared" % "4.12.4.1" cross (CrossVersion.for3Use2_13) exclude ("com.lihaoyi", "sourcecode_2.13") exclude (
        "org.jline",
        "jline"
      ),
      "org.scala-lang.modules" % "scala-asm" % "9.7.1-scala-1",
      // "ch.epfl.scala" %% "tasty-query" % "1.4.0",
      // "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      // "fr.inria.gforge.spoon" % "spoon-core" % "11.1.1-beta-9",
      // "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.26.2",
      // "org.soot-oss" % "sootup.core" % sootupVersion,
      // "org.soot-oss" % "sootup.java.core" % sootupVersion,
      // "org.soot-oss" % "sootup.java.sourcecode" % sootupVersion,
      // "org.soot-oss" % "sootup.java.bytecode" % sootupVersion,
      // "org.soot-oss" % "sootup.jimple.parser" % sootupVersion,
      // "org.soot-oss" % "sootup.callgraph" % sootupVersion,
      // "org.soot-oss" % "sootup.analysis" % sootupVersion,
      "org.mozilla" % "rhino" % "1.7.15"
      // suppose to support normal jvm https://github.com/oracle/graaljs/blob/master/docs/user/RunOnJDK.md
      // https://www.graalvm.org/latest/reference-manual/native-image/guides/build-polyglot-native-executable/
      // "org.graalvm.polyglot" % "polyglot" % graalvmVersion,
      // "org.graalvm.polyglot" % "js" % graalvmVersion
      // "org.bytedeco" % "llvm-platform" % "18.1.8-1.5.11-SNAPSHOT" // no: no enough memory to build it with native image
    )
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .dependsOn(utils, reader, syntax, pretty, tyck)
  .settings(
    name := "core",
    assembly / assemblyOutputPath := file("target") / "chester-core.jar",
    commonSettings
    // cpsSettings,
  )
  .jvmSettings(commonJvmLibSettings)

lazy val typednode = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typednode"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/node_sjs1_3/22.5.5-6bc698/srcs/node_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/node_sjs1_3/22.5.5-6bc698/jars/node_sjs1_3.jar"
    )
  )
lazy val typedstd = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedstd"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/std_sjs1_3/4.3-5d95db/srcs/std_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/std_sjs1_3/4.3-5d95db/jars/std_sjs1_3.jar"
    )
  )
lazy val typedundici = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedundici"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/undici-types_sjs1_3/6.19.8-4dee3c/srcs/undici-types_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/undici-types_sjs1_3/6.19.8-4dee3c/jars/undici-types_sjs1_3.jar"
    )
  )
lazy val typedxterm = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedxterm"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/xterm__xterm_sjs1_3/5.5.0-951203/srcs/xterm__xterm_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/xterm__xterm_sjs1_3/5.5.0-951203/jars/xterm__xterm_sjs1_3.jar"
    )
  )
lazy val typedcsstype = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedcsstype"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/csstype_sjs1_3/3.1.3-3d3924/srcs/csstype_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/csstype_sjs1_3/3.1.3-3d3924/jars/csstype_sjs1_3.jar"
    )
  )
lazy val typednext = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("js-typings/typednext"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/next_sjs1_3/11.1.4-68204a/srcs/next_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/next_sjs1_3/11.1.4-68204a/jars/next_sjs1_3.jar"
    )
  )
lazy val typedproptypes = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedproptypes"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/prop-types_sjs1_3/15.7.13-49b294/srcs/prop-types_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/prop-types_sjs1_3/15.7.13-49b294/jars/prop-types_sjs1_3.jar"
    )
  )
lazy val typedreactdom = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedreactdom"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/react-dom_sjs1_3/18.3.0-d84423/srcs/react-dom_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/react-dom_sjs1_3/18.3.0-d84423/jars/react-dom_sjs1_3.jar"
    )
  )
lazy val typedreact = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedreact"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/react_sjs1_3/18.3.7-ca07dd/srcs/react_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/react_sjs1_3/18.3.7-ca07dd/jars/react_sjs1_3.jar"
    )
  )
lazy val typedxtermreadline = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedxtermreadline"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/xterm-readline_sjs1_3/1.1.1-a2b93f/srcs/xterm-readline_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/xterm-readline_sjs1_3/1.1.1-a2b93f/jars/xterm-readline_sjs1_3.jar"
    )
  )
lazy val typedxterm2 = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedxterm2"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/xterm_sjs1_3/5.3.0-80131f/srcs/xterm_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/xterm_sjs1_3/5.3.0-80131f/jars/xterm_sjs1_3.jar"
    )
  )
lazy val typedxtermpty = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedxtermpty"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/xterm-pty_sjs1_3/0.9.6-3b34b4/srcs/xterm-pty_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/xterm-pty_sjs1_3/0.9.6-3b34b4/jars/xterm-pty_sjs1_3.jar"
    )
  )
lazy val typedvscodeJsonrpc = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedvscodeJsonrpc"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/vscode-jsonrpc_sjs1_3/8.2.0-224d90/srcs/vscode-jsonrpc_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/vscode-jsonrpc_sjs1_3/8.2.0-224d90/jars/vscode-jsonrpc_sjs1_3.jar"
    )
  )

lazy val typedvscodeLanguageserverProtocol = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedvscodeLanguageserverProtocol"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver-protocol_sjs1_3/3.17.5-a68af5/srcs/vscode-languageserver-protocol_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver-protocol_sjs1_3/3.17.5-a68af5/jars/vscode-languageserver-protocol_sjs1_3.jar"
    )
  )

lazy val typedvscodeLanguageserverTextdocument = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedvscodeLanguageserverTextdocument"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver-textdocument_sjs1_3/1.0.12-0aa4d4/srcs/vscode-languageserver-textdocument_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver-textdocument_sjs1_3/1.0.12-0aa4d4/jars/vscode-languageserver-textdocument_sjs1_3.jar"
    )
  )

lazy val typedvscodeLanguageserverTypes = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedvscodeLanguageserverTypes"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver-types_sjs1_3/3.17.5-6935e7/srcs/vscode-languageserver-types_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver-types_sjs1_3/3.17.5-6935e7/jars/vscode-languageserver-types_sjs1_3.jar"
    )
  )

lazy val typedvscodeLanguageserver = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .in(file("js-typings/typedvscodeLanguageserver"))
  .settings(commonVendorSettings)
  .jsSettings(
    Compile / packageSrc := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver_sjs1_3/9.0.1-28aecf/srcs/vscode-languageserver_sjs1_3-sources.jar"
    ),
    Compile / packageBin := file(
      "js-typings/local/org.scalablytyped/vscode-languageserver_sjs1_3/9.0.1-28aecf/jars/vscode-languageserver_sjs1_3.jar"
    )
  )

// Update the jsTypings project to include the new dependencies
lazy val jsTypings = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("js-typings"))
  // Remove comments to generate typings again
  // .jsEnablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    commonVendorSettings
  )
  .jsConfigure(
    _.dependsOn(
      typednode.js,
      typedstd.js,
      typedundici.js,
      typedxterm.js,
      typedcsstype.js,
      typednext.js,
      typedproptypes.js,
      typedreactdom.js,
      typedreact.js,
      typedxtermreadline.js,
      typedxterm2.js,
      typedxtermpty.js,
      typedvscodeJsonrpc.js,
      typedvscodeLanguageserverProtocol.js,
      typedvscodeLanguageserverTextdocument.js,
      typedvscodeLanguageserverTypes.js,
      typedvscodeLanguageserver.js
    )
  )
  .jsSettings(
    resolvers += Resolver.file("local-ivy2", file("js-typings/local"))(
      Resolver.ivyStylePatterns
    ),
    libraryDependencies ++= Seq(
      "com.olvind" %%% "scalablytyped-runtime" % "2.4.2",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0"
    ),
    libraryDependencies ++= Seq(
      "org.scalablytyped" %%% "node" % "22.5.5-6bc698" % Compile,
      "org.scalablytyped" %%% "std" % "4.3-5d95db" % Compile,
      "org.scalablytyped" %%% "undici-types" % "6.19.8-4dee3c" % Compile,
      "org.scalablytyped" %%% "xterm__xterm" % "5.5.0-951203" % Compile,
      "org.scalablytyped" %%% "csstype" % "3.1.3-3d3924" % Compile,
      "org.scalablytyped" %%% "next" % "11.1.4-68204a" % Compile,
      "org.scalablytyped" %%% "prop-types" % "15.7.13-49b294" % Compile,
      "org.scalablytyped" %%% "react-dom" % "18.3.0-d84423" % Compile,
      "org.scalablytyped" %%% "react" % "18.3.7-ca07dd" % Compile,
      "org.scalablytyped" %%% "xterm-readline" % "1.1.1-a2b93f" % Compile,
      "org.scalablytyped" %%% "xterm" % "5.3.0-80131f" % Compile,
      "org.scalablytyped" %%% "xterm-pty" % "0.9.6-3b34b4" % Compile,
      "org.scalablytyped" %%% "vscode-jsonrpc" % "8.2.0-224d90" % Compile,
      "org.scalablytyped" %%% "vscode-languageserver-protocol" % "3.17.5-a68af5" % Compile,
      "org.scalablytyped" %%% "vscode-languageserver-textdocument" % "1.0.12-0aa4d4" % Compile,
      "org.scalablytyped" %%% "vscode-languageserver-types" % "3.17.5-6935e7" % Compile,
      "org.scalablytyped" %%% "vscode-languageserver" % "9.0.1-28aecf" % Compile
    )
    /*
    Compile / npmDependencies ++= Seq(
      "vscode-languageserver" -> "9.0.1",
      "vscode-languageserver-textdocument" -> "1.0.12",
      "vscode-languageserver-protocol" -> "3.17.5",
    ),
    Compile / npmDependencies ++= Seq(
      "@types/node" -> "22.5.5",
      "@xterm/xterm" -> "5.5.0",
      "@types/react" -> "18.3.7",
      "@types/react-dom" -> "18.3.0",
      "next" -> "11.1.4", // next.js 14/12 breaks scalablytyped
      "xterm-readline" -> "1.1.1",
      "xterm-pty" -> "0.9.6", // use old version because of https://github.com/mame/xterm-pty/issues/35
    ),
     */
  )

lazy val platform0 = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("platform0"))
  .dependsOn(core, compiler)
  .jsConfigure(
    _.dependsOn(jsTypings.js)
  )
  .settings(
    name := "chester",
    assembly / assemblyOutputPath := file("target") / "chester-common.jar",
    commonSettings
  )
  .jvmSettings(
    commonJvmLibSettings
  )
  .nativeSettings(
    scalacOptions ++= (if (supportNativeBuildForTermux)
                         Seq(
                           "-Xmacro-settings:com.eed3si9n.ifdef.declare:scalaNativeForTermux"
                         )
                       else Seq())
  )
  .jsSettings(
  )

addCommandAlias("cliReadline", "set ThisBuild / enableCliReadline := true;")
addCommandAlias("cliSimple", "set ThisBuild / enableCliReadline := false;")

val enableCliReadline =
  settingKey[Boolean]("Flag to enable or disable cliReadline")
ThisBuild / enableCliReadline := false
val windows: Boolean = System.getProperty("os.name").toLowerCase.contains("win")
val unix: Boolean = !windows

val jlineVersion = "3.28.0"
lazy val cli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("cli"))
  .jvmEnablePlugins(NativeImagePlugin)
  .enablePlugins(BuildInfoPlugin) // Enable the BuildInfoPlugin
  .dependsOn(platform0, platform, compiler)
  .settings(
    Compile / mainClass := Some("chester.cli.Main"),
    assembly / assemblyOutputPath := file("target") / "chester.jar",
    libraryDependencies ++= Seq(
      // "com.github.alexarchambault" %%% "case-app" % "2.1.0-M29",
      "com.github.scopt" %%% "scopt" % "4.1.0"
    ),
    commonSettings,
    buildInfoKeys := Seq[BuildInfoKey](version), // Specify the keys to include
    buildInfoPackage := "chester", // Set the package for the generated object
    buildInfoObject := "BuildInfo" // Set the object name
  )
  .jvmSettings(
    commonJvmLibSettings,
    nativeImageOutput := file("target") / "chester",
    graalvmSettings,
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % jlineVersion,
      "org.jline" % "jline-terminal" % jlineVersion,
      // "org.jline" % "jline-terminal-jansi" % jlineVersion,
      "org.jline" % "jline-terminal-jni" % jlineVersion,
      // "org.jline" % "jline-terminal-jna" % jlineVersion,
      // "org.jline" % "jline-terminal-ffm" % jlineVersion,
      // "org.jline" % "jline-native" % jlineVersion,
      "org.jline" % "jline-reader" % jlineVersion
      // "org.jline" % "jline-style" % jlineVersion,
      // "org.jline" % "jline-remote-ssh" % jlineVersion,
      // "org.jline" % "jline-remote-telnet" % jlineVersion,
      // "org.jline" % "jline-builtins" % jlineVersion,
      // "org.jline" % "jline-console" % jlineVersion,
      // "org.jline" % "jline-groovy" % jlineVersion,
      // "org.jline" % "jline-console-ui" % jlineVersion
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
  )
  .nativeSettings(
    libraryDependencies ++= Seq(
      // "io.github.edadma" %%% "readline" % "0.1.3"
    ),
    scalacOptions ++= (if ((ThisBuild / enableCliReadline).value)
                         Seq(
                           "-Xmacro-settings:com.eed3si9n.ifdef.declare:readline"
                         )
                       else Seq())
  )

lazy val js = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("js"))
  .dependsOn(platform0)
  .settings(
    commonSettings
  )
  .jsSettings(
  )

lazy val site = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("site"))
  .dependsOn(js)
  .settings(
    commonSettings
  )
  .jsSettings(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
    /*
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    },
     */
    // libraryDependencies += "com.github.japgolly.scalajs-react" %%% "core" % "2.1.2",
    // libraryDependencies += "me.shadaj" %%% "slinky-core" % "0.7.4"
  )

lazy val docs = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Full)
  .in(file("docs"))
  .dependsOn(js)
  .settings(
    commonSettings
  )
  .jsSettings(
  )

lazy val lsp = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("lsp"))
  .jvmEnablePlugins(NativeImagePlugin)
  // .enablePlugins(SbtProguard)
  .dependsOn(buildProtocol)
  .settings(
    libraryDependencies ++= Seq(
    ),
    Compile / mainClass := Some("chester.lsp.Main"),
    libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.24.0-SNAPSHOT",
    assembly / assemblyOutputPath := file("target") / "chester-lsp.jar",
    nativeImageOutput := file("target") / "chester-lsp",
    commonSettings
    // proguard is breaking the build
    /*
    // https://stackoverflow.com/questions/39655207/how-to-obfuscate-fat-scala-jar-with-proguard-and-sbt/39663793#39663793
    // Proguard settings
    Proguard / proguardOptions ++= Seq(
      "-dontoptimize",
      "-keepattributes *Annotation*",
      "-keep public class * { public static void main(java.lang.String[]); }",
      "-keep public class chester.**,org.eclipse.lsp4j.** { *; }",
      "-dontnote", "-dontwarn", //"-ignorewarnings"
    ),
    Proguard / proguardVersion := "7.5.0",
    Proguard / proguard / javaOptions := Seq("-Xmx4G"),
    Proguard / proguardInputs := Seq((assembly / assemblyOutputPath).value),
    Proguard / proguardLibraries := (Proguard / proguard / javaHome).value.toSeq,
    Proguard / proguardInputFilter := (_ => None),
    Proguard / proguardMerge := false,
    Proguard / proguard := (Proguard / proguard).dependsOn(assembly).value,
    Proguard / artifactPath := file("target") / "chester-lsp.jar",
     */
  )
  .jvmSettings(
    graalvmSettings
  )

lazy val buildProtocol = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("build-protocol"))
  .dependsOn(platform0)
  .settings(
    name := "build-protocol",
    commonSettings,
    libraryDependencies ++= Seq(
      // "ch.epfl.scala" %%% "bsp4s" % "2.2.0-M4.TEST" cross (CrossVersion.for3Use2_13) exclude("com.lihaoyi", "sourcecode_2.13") exclude("org.typelevel", "cats-core_2.13") exclude("org.typelevel", "cats-kernel_2.13"),
      // "com.lihaoyi" %%% "sourcecode" % "0.4.3-M1",
      // "org.typelevel" %%% "cats-core" % "2.12.0",
      // "org.typelevel" %%% "cats-kernel" % "2.12.0",
      "org.log4s" %%% "log4s" % "1.10.0",
      "org.slf4j" % "slf4j-api" % "2.0.16",
      "org.slf4j" % "slf4j-simple" % "2.0.16",
      "ch.epfl.scala" % "bsp4j" % "2.2.0-M4.TEST"
    )
  )
  .jvmSettings(commonJvmLibSettings)

val jgitVersion = "7.1.0.202411261347-r"
lazy val buildTool = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("build-tool"))
  .jvmEnablePlugins(NativeImagePlugin)
  .dependsOn(buildProtocol)
  .settings(
    name := "build-tool",
    Compile / mainClass := Some("chester.build.Main"),
    assembly / assemblyOutputPath := file("target") / "chester-build.jar",
    nativeImageOutput := file("target") / "chester-build",
    commonSettings,
    graalvmSettings,
    libraryDependencies ++= Seq(
      "org.eclipse.jgit" % "org.eclipse.jgit" % jgitVersion,
      "org.eclipse.jgit" % "org.eclipse.jgit.lfs" % jgitVersion
    )
  )

lazy val eval = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("eval"))
  .dependsOn(tyckBase)
  .settings(
    commonSettings
  )
  .jvmSettings(commonJvmLibSettings)

lazy val interpreter = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("interpreter"))
  .dependsOn(eval, reader, tyck)
  .settings(commonSettings)
  // https://github.com/b-studios/scala-graal-truffle-example/blob/c2747a6eece156f878c5b934116aaa00a2cd6311/build.sbt
  .settings(
    assembly / assemblyOutputPath := file("target") / "chester-interpreter.jar"
  )
  .jvmSettings(
    libraryDependencies += "org.jetbrains" % "annotations" % "26.0.1",
    assembly / test := {},
    assembly / assemblyExcludedJars := {
      val cp = (assembly / fullClasspath).value
      // https://stackoverflow.com/questions/41894055/how-to-exclude-jar-in-final-sbt-assembly-plugin
      cp filter { f =>
        val path = f.data.toString
        (path contains "com.oracle.truffle") ||
        (path contains "org.graalvm")
      }
    },
    // We fork the JVM to pass the Java Options
    Compile / run / fork := true,
    javaOptions ++= Seq(
      "-Dgraal.Dump=Truffle:1",
      "-Dgraal.TruffleBackgroundCompilation=false",
      "-Dgraal.TraceTruffleCompilation=true",
      "-Dgraal.TraceTruffleCompilationDetails=true",
      "-XX:-UseJVMCIClassLoader"
    ),
    dependOnGraal,
    Compile / javacOptions ++= {
      val cp = (Compile / dependencyClasspath).value.map(_.data)
      val processorJars = cp.filter(_.getName.contains("truffle-dsl-processor"))
      val processorPath = processorJars.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

      Seq(
        "-Xlint:unchecked",
        "-processorpath",
        processorPath,
        "--release",
        "17"
      )
    }
  )

// Useful for Intellij IDEA development purposes
lazy val allprojects = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("project/allprojects"))
  .dependsOn(lsp, cli, platform, core, interpreter)
  .settings(
    commonSettings
  )

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("project/root"))
  .aggregate(
    allprojects,
    ironNative,
    spireNative,
    typednode,
    typedstd,
    typedundici,
    typedxterm,
    typedcsstype,
    typednext,
    typedproptypes,
    typedreactdom,
    typedreact,
    typedxtermreadline,
    typedxterm2,
    typedxtermpty,
    typedvscodeJsonrpc,
    typedvscodeLanguageserverProtocol,
    typedvscodeLanguageserverTextdocument,
    typedvscodeLanguageserverTypes,
    typedvscodeLanguageserver,
    kiamaCore,
    effektKiama,
    jsTypings,
    utils,
    reader,
    compiler,
    compiler213,
    syntax,
    err,
    pretty,
    tyck,
    tyckBase,
    platform,
    jsForJvm,
    core,
    platform0,
    cli,
    lsp,
    buildProtocol,
    buildTool,
    eval,
    interpreter,
    js,
    site,
    docs
  )
  .settings(
    scalaVersion := scala3Version
  )

Global / excludeLintKeys ++= Set[SettingKey[_]](
  cli.jvm / nativeImageJvm,
  cli.jvm / nativeImageVersion,
  cli.js / nativeImageJvm,
  cli.js / nativeImageVersion,
  cli.native / nativeImageJvm,
  cli.native / nativeImageVersion,
  lsp.jvm / nativeImageJvm,
  lsp.jvm / nativeImageVersion
)
