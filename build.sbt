// reads env: NATIVE_IMAGE_OPTIONS, VERSION

val scala3Nightly = "3.7.2-RC1-bin-20250507-c85982c-NIGHTLY"
val scala3Version = "3.7.1-RC1"
val scala3Lib = "3.6.4"
val scala2Version = "2.13.16"
val scala2Beta = "2.13.17-M1"

val graalVm = "graalvm-java24"
val graalJdkVersion = "24.0.1"
val graalvmVersion = "24.2.1"

ThisBuild / version := sys.env.getOrElse("VERSION", "0.0.33")
ThisBuild / organization := "com.github.chester-lang"

import org.scalajs.linker.interface.OutputPatterns
import sbt.librarymanagement.InclExclRule
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.Platform

import scala.scalanative.build.*
import sbt.complete.DefaultParsers.*

import scala.sys.process.*
import scala.util.Using

addCommandAlias("testAll", ";rootJVM/test; rootJS/test; rootNative/test")
addCommandAlias("testWinCi", ";rootJVM/test; rootJS/test; rootNative/compile") // we have some bugs on ci
addCommandAlias("updates", "reload plugins; dependencyUpdates; reload return; dependencyUpdates;")
addCommandAlias("format0", "scalafmtAll; scalafmtSbt; rootJVM/scalafixAll;")
addCommandAlias("format", "scalafmtAll; scalafmtSbt; rootJVM/scalafixAll; rootJS/scalafixAll; rootNative/scalafixAll;")

val scalafixRules = Seq(
  "AddExplicitImplicitTypes",
  "AddLambdaParamParentheses",
  "CatsToValid",
  "CirceCodec",
  "CollectHead",
  "CollectHeadOption",
  "CompareSameValue",
  "DirectoryAndPackageName",
  "DiscardValue",
  // "DuplicateWildcardImport",
  // "EitherFold",
  "EitherGetOrElse",
  "EitherMap",
  "EtaExpand",
  "ExplicitImplicitTypes",
  "ExtendsProductWithSerializable",
  "FileNameConsistent",
  "FilterNot",
  "FilterSize",
  "FinalObjectWarn",
  "FlatMapCollect",
  "FlatTraverse",
  "ForTupleMatch",
  "GroupMap",
  "ImplicitValueClass",
  "IncorrectScaladocParam",
  "InterpolationToString",
  "InterpolationToStringWarn",
  "IsEmptyNonEmpty",
  "JavaURLConstructorsWarn",
  "KeySet",
  "KindProjector",
  "KindProjectorScala3TypeLambda",
  "LambdaParamParentheses",
  "LazyZip",
  "MapDistinctSize",
  "MapFlattenFlatMap",
  "MapSequenceTraverse",
  "MapToForeach",
  // "MatchParentheses",
  "NamedParamOrder",
  "NoElse",
  "ObjectFinal",
  "ObjectSelfType",
  "OptionContains",
  "OptionFilter",
  "OptionForallExists",
  "OptionGetOrElse",
  "OptionGetWarn",
  "OptionMapFlatMap",
  "OptionMatchToRight",
  "OptionOrElse",
  "OptionWhenUnless",
  "PartialFunctionCondOpt",
  "RedundantCaseClassVal",
  "RemoveEmptyObject",
  // "RemoveIf",
  "RemovePureEff",
  // "RemoveSamePackageImport",
  "RemoveStringInterpolation",
  "RemoveUselessParamComments",
  "RepeatedRewrite",
  "ReplaceFill",
  // "ReplacePlaceholder",
  "ReplaceSymbolLiterals",
  "ReuseInstances",
  "SameParamOverloading",
  // "Scala3ImportRewrite",
  // "Scala3ImportWarn",
  "Scala3Placeholder",
  "ScalaApp",
  "ScalazEitherInfix",
  // "SeparateEachFileRewrite",
  // "SeparateEachFileWarn",
  "SimplifyForYield",
  "SizeToLength",
  "SlickFilter",
  "StringFormatToInterpolation",
  // "SyntacticOrganizeImports",
  "ThrowableToNonFatal",
  "UnmooredDocComment",
  "UnnecessaryCase",
  "UnnecessaryMatch",
  "UnnecessarySort",
  "UnnecessarySortRewrite",
  "UnusedConstructorParams",
  "UnusedSelfType",
  "UnusedTypeParams",
  "UselessParamCommentsWarn",
  "UsingParamAnonymous",
  "UsingParamAnonymousConstructor",
  "WithFilter",
  "WithLeftWithRight"
).map(x => "dependency:" + x + "@com.github.xuwei-k:scalafix-rules:0.6.2") ++ Seq(
  "dependency:EmptyCollectionsUnified@io.github.ghostbuster91.scalafix-unified:unified:0.0.9",
  "dependency:NonCaseException@net.pixiv:scalafix-pixiv-rule:4.5.3",
  "dependency:NeedMessageExtendsRuntimeException@net.pixiv:scalafix-pixiv-rule:4.5.3",
  "dependency:CheckIsEmpty@net.pixiv:scalafix-pixiv-rule:4.5.3",
  "dependency:UnifiedArrow@net.pixiv:scalafix-pixiv-rule:4.5.3",
  "dependency:UnnecessarySemicolon@net.pixiv:scalafix-pixiv-rule:4.5.3"
)

addCommandAlias("fixmore", scalafixRules.map(rule => s"""eval println("Applying rule: $rule"); rootJVM/scalafixAll $rule""").mkString("; "))

addCommandAlias("fmt", ";scalafmtAll; scalafmtSbt")
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
    file("js-for-python"),
    file("js-for-lua")
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
lazy val outdated = inputKey[Unit]("Run pnpm outdated in site and vscode folders")
outdated := {
  val log = streams.value.log
  val folders = Seq(
    file("site"),
    file("vscode"),
    file("js-for-python"),
    file("js-for-lua")
  )

  folders.foreach { dir =>
    if (dir.exists()) {
      log.info(s"Checking outdated dependencies in ${dir.getName}...")
      Process("pnpm outdated", dir) ! log
    } else {
      log.warn(s"Directory ${dir.getName} does not exist, skipping")
    }
  }

  log.success("Finished checking all dependencies")
}

val defaultNativeImageOptions = Seq(
  // "-H:-CheckToolchain",
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

val jdk17: Boolean = false
val jdk21: Boolean = false
val javaOutputVersion: String = "8"
val testJavaOutputVersion: String = "17"

def commonSettings0 = Seq(
  // Workaround for Metals: disable BSP for native/js targets to prevent compilation issues
  // See: https://github.com/scalameta/metals-feature-requests/issues/13
  bspEnabled := {
    val platform = crossProjectPlatform.?.value.getOrElse(JVMPlatform)
    platform == JVMPlatform
  },
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
      "-feature",
      "-experimental",
      "--preview"
    ),
  // required by scalafix?
  scalacOptions ++= Seq("-Wunused:all"), // "-Xlint:adapted-args"
  scalacOptions ++= Seq("-rewrite", "-source", "3.7-migration"),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.1.1" % Test,
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    "org.scalatest" %%% "scalatest-funsuite" % "3.2.19" % Test,
    "org.scalatest" %%% "scalatest-shouldmatchers" % "3.2.19" % Test,
    "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test,
    "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test
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
    ExclusionRule("org.scala-native", "test-interface_native0.5_2.13"),
    ExclusionRule("org.scala-lang.modules", "scala-collection-compat_2.13")
  )
)
def commonSettings = commonSettings0 ++ Seq(
  scalaVersion := scala3Version
)
def commonLibSettings = commonSettings0 ++ Seq(
  scalaVersion := scala3Lib
)
def scala2Common = Seq(
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
  // required by scalafix?
  scalacOptions ++= Seq("-Xlint:adapted-args"), // "-Wunused:all",
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "1.1.1" % Test cross CrossVersion.for2_13Use3,
    "org.scalatest" %%% "scalatest" % "3.2.19" % Test cross CrossVersion.for2_13Use3,
    "org.scalatest" %%% "scalatest-funsuite" % "3.2.19" % Test cross CrossVersion.for2_13Use3,
    "org.scalatest" %%% "scalatest-shouldmatchers" % "3.2.19" % Test cross CrossVersion.for2_13Use3,
    "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test cross CrossVersion.for2_13Use3,
    "org.scalacheck" %%% "scalacheck" % "1.18.1" % Test cross CrossVersion.for2_13Use3
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
def scala2JVM = Seq(
  // scalaVersion := scala2Beta,
// semanticdbEnabled := false // not supported for beta
)
def commonVendorSettings = Seq(
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  scalaVersion := scala3Lib,
  scalacOptions ++= Seq("-java-output-version", javaOutputVersion),
  scalacOptions += "-nowarn"
)
def scala2VendorSettings = Seq(
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  scalaVersion := scala2Version,
  scalacOptions ++= Seq("-java-output-version", javaOutputVersion),
  scalacOptions += "-nowarn"
)
def cpsSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin(
    "com.github.rssh" %% "dotty-cps-async-compiler-plugin" % "0.9.23"
  )
)
def commonJvmSettings = Seq(
  scalacOptions ++= (if (jdk17) Seq("-Xmacro-settings:com.eed3si9n.ifdef.declare:jdk17") else Seq()),
  scalacOptions ++= (if (jdk21) Seq("-Xmacro-settings:com.eed3si9n.ifdef.declare:jdk21") else Seq()),
  Compile / scalacOptions ++= Seq("-java-output-version", javaOutputVersion),
  Test / scalacOptions ~= { opts =>
    opts.filterNot(_ == "-java-output-version").filterNot(_ == javaOutputVersion) ++ Seq("-java-output-version", testJavaOutputVersion)
  }
)
def jvmScala3Settings = Seq(
  scalaVersion := scala3Nightly
)

val NativeImageOptions = sys.env.get("NATIVE_IMAGE_OPTIONS").map(_.split(" ").toList).getOrElse(List[String]())

def graalvmSettings = Seq(
  nativeImageVersion := graalJdkVersion,
  nativeImageOptions ++= defaultNativeImageOptions,
  nativeImageOptions ++= NativeImageOptions,
  nativeImageJvm := graalVm
)

// https://scalablytyped.org/docs/remotecache
// sbt stPublishCache
Global / stRemoteCache := {
  import java.io.File
  import scala.io.Source
  import scala.util.{Try, Success, Failure}

  val homeDir = System.getProperty("user.home")
  val configFile = new File(homeDir, "chester-st-cache")

  // For custom SSH ports, add a host entry in ~/.ssh/config like:
  // Host myserver
  //   HostName xx
  //   User xx
  //   Port xx
  // Then use "myserver:/path/to/cache" as the pushAddress
  val pushAddress = Try {
    if (configFile.exists) {
      Using(Source.fromFile(configFile))(source => source.getLines().mkString.trim).get
    } else {
      "user@server.com:/path/to/cache"
    }
  } match {
    case Success(addr) if addr.nonEmpty => addr
    case _                              => "user@server.com:/path/to/cache"
  }

  RemoteCache.Rsync(
    push = pushAddress,
    pull = new java.net.URI("https://cache.the-lingo.org/st")
  )
}

commonSettings

lazy val bump = inputKey[Unit]("Bump version in multiple files")
bump := {
  val args: Seq[String] = spaceDelimited("<new_version>").parsed
  if (args.length != 1) {
    println("Usage: bump <new_version>")
  } else {
    val newVersion = args.head
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
    val newScalaVersion = args.head
    val oldScalaVersion = scalaVersion.value
    val filesToUpdate = Seq(
      file("build.sbt"),
      file(".github/workflows/build.yml"),
      file("docs/dev.sh"),
      file("idea-plugin/build.sbt"),
      file("site/package.json"),
      file("cli/package.json"),
      file("packages/base/package.json"),
      file("js-for-python/index.js"),
      file("js-for-lua/index.js")
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

val supportNativeBuildForTermux =
  System.getProperty("java.vm.vendor") == "Termux" || System.getProperty("sun.boot.library.path", "").startsWith("/data/data/com.termux")

ThisBuild / nativeConfig ~= ((System.getProperty("os.name").toLowerCase, System.getProperty("os.arch").toLowerCase) match {
  case (mac, _) if mac.contains("mac") => // mac has some bugs with optimizations
    _.withGC(GC.commix)
  /*
  [error] /usr/bin/ld: /tmp/lto-llvm-7d968c.o: relocation R_AARCH64_ADR_PREL_PG_HI21 against symbol `__stack_chk_guard@@GLIBC_2.17' which may bind externally can not be used when making a shared object; recompile with -fPIC
  [error] /usr/bin/ld: /tmp/lto-llvm-7d968c.o(.text.MutatorThreads_init+0x8): unresolvable R_AARCH64_ADR_PREL_PG_HI21 relocation against symbol `__stack_chk_guard@@GLIBC_2.17'
  [error] /usr/bin/ld: final link failed: bad value
  [info] Total (36687 ms)
   */
  // Archlinux aarch64 Virtual Machine on Apple Silicon: LTO is broken too
  case (linux, "aarch64") if linux.contains("linux") =>
    _.withMode(Mode.releaseFast)
      .withGC(GC.commix)
  case _ =>
    _.withLTO(LTO.thin)
      .withMode(Mode.releaseFast)
      .withGC(GC.commix)
})

ThisBuild / nativeConfig ~= (if (supportNativeBuildForTermux) {
                               _.withMultithreading(false).withGC(GC.immix)
                             } else x => x)

ThisBuild / nativeConfig ~= (if (supportNativeBuildForTermux) {
                               _.withCompileOptions(_ :+ "-D_POSIX_C_SOURCE=200809L")
                                 .withLinkingOptions(_ :+ "-landroid-posix-semaphore")
                             } else x => x)
// original kiama-core
lazy val kiamaCore = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("vendor/kiama-core"))
  .settings(
    commonVendorSettings
  )
  .disablePlugins(ScalafixPlugin)
  .jvmSettings(commonJvmSettings)

// commit 52b3692bdfe01ef6c645380b02595a9c60a9725b, core & util & platform & macros, main only, no tests
// rewrite by scalac with 3.4-migration
// needed project/GenProductTypes.scala
lazy val genProductTypes = TaskKey[Seq[File]](
  "gen-product-types",
  "Generates several type classes for Tuple2-22."
)
lazy val spireNative = crossProject(NativePlatform)
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
      "org.typelevel" %%% "algebra-laws" % "2.13.0"
    )
  )
  .nativeSettings(
  )

// https://codeberg.org/sciss/scala-stm/commit/3244edf13c41f22ff8b45143186745e9eb469220
// test currently removed, will be added later
// js: shouldParkAfterFailedAcquire patched the  do while loop; AtomicIntegerArray val length instead of `length` and removed length() method
// shared scala.concurrent.stm.Handle var meta: Long changed to var meta1: Long and  def meta: Long  def meta_=(value: Long):Unit so that js's CCSTMRefs.scala can override it
lazy val scalaSTM = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("vendor/scala-stm"))
  .settings(
    commonVendorSettings,
  libraryDependencies ++= Seq(
    "org.scalatest"     %%% "scalatest"  % "3.2.19"     % Test,
    "org.scalatestplus" %%  "junit-5-10" % "3.2.19.1" % Test,
    "org.junit.jupiter"             %   "junit-jupiter-api"      % "5.10.5"         % Test,
  ),
  )
  .disablePlugins(ScalafixPlugin)
  .jvmSettings(commonJvmSettings)


val AIRFRAME_VERSION = "2025.1.10"
val ironVersion = "3.0.1"

// split modules trying to increase incremental compilation speed
lazy val utils = useSpire(
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("utils"))
    .settings(
      commonLibSettings,
      libraryDependencies ++= Seq(
        "io.github.iltotore" %%% "iron" % ironVersion,
        "io.github.iltotore" %%% "iron-cats" % ironVersion,
        "io.github.iltotore" %%% "iron-upickle" % ironVersion exclude ("com.lihaoyi", "upickle_3"),
        "org.typelevel" %%% "cats-core" % "2.13.0",
        "org.typelevel" %%% "cats-free" % "2.13.0",
        "com.lihaoyi" %%% "pprint" % "0.9.0",
        "com.lihaoyi" %%% "upickle" % "4.1.0",
        "com.lihaoyi" %%% "fansi" % "0.5.0",
        "com.lihaoyi" %%% "fastparse" % "3.1.1",
        // "com.lihaoyi" %%% "scalatags" % "0.13.1",
        // "com.github.rssh" %%% "dotty-cps-async" % "0.9.23",
        // "io.getkyo" %%% "kyo-prelude" % "0.18.0",
        // "io.getkyo" %%% "kyo-core" % "0.18.0",
        // "io.getkyo" %%% "kyo-direct" % "0.18.0",
        // "io.getkyo" %%% "kyo-data" % "0.18.0",
        "org.scala-graph" %%% "graph-core" % "2.0.3",
        "com.outr" %%% "scribe" % "3.16.1",
        "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.35.3",
        "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.35.3",
        "org.wvlet.airframe" %%% "airframe-log" % AIRFRAME_VERSION // Logging
        /*
        "org.wvlet.airframe" %%% "airframe" % AIRFRAME_VERSION, // Dependency injection
        "org.wvlet.airframe" %%% "airframe-codec" % AIRFRAME_VERSION, // MessagePack-based schema-on-read codec
        "org.wvlet.airframe" %%% "airframe-control" % AIRFRAME_VERSION, // Library for retryable execution
        "org.wvlet.airframe" %%% "airframe-json" % AIRFRAME_VERSION, // Pure Scala JSON parser
        "org.wvlet.airframe" %%% "airframe-msgpack" % AIRFRAME_VERSION, // Pure-Scala MessagePack
        "org.wvlet.airframe" %%% "airframe-metrics" % AIRFRAME_VERSION, // Metrics units
        "org.wvlet.airframe" %%% "airframe-rx" % AIRFRAME_VERSION, // ReactiveX interface
        "org.wvlet.airframe" %%% "airframe-surface" % AIRFRAME_VERSION, // Object surface inspector
        "org.wvlet.airframe" %%% "airframe-ulid" % AIRFRAME_VERSION // ULID generator
         */
      )
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "os-lib" % "0.11.4"
      ),
      commonJvmSettings,
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
        "com.lihaoyi" %%% "os-lib" % "0.11.4"
      ),
      libraryDependencies ++= Seq(
        "org.scala-js" %% "scalajs-stubs" % "1.1.0"
      )
    )
    .jsSettings(
    )
    .nativeConfigure(_.dependsOn(scalaSTM.native))
    .jsConfigure(_.dependsOn(scalaSTM.js))
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.scala-stm" %%% "scala-stm" % "0.11.1"
      ),
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
  .jvmSettings(commonJvmSettings)

lazy val reader = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("reader"))
  .dependsOn(utils, syntax, compatibility % Test, testCommon % Test)
  .settings(
    commonSettings
  )
  .jvmSettings(commonJvmSettings)

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
    .jvmSettings(commonJvmSettings)
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
  .jvmSettings(commonJvmSettings)

lazy val testCommon = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("test-common"))
  .dependsOn(compatibility)
  .settings(
    commonSettings
  )
  .jvmSettings(commonJvmSettings)

lazy val semantics = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("semantics"))
  .dependsOn(utils, syntax, err, compatibility % Test, testCommon % Test, reader % Test)
  .settings(
    commonSettings
  )
  .jvmSettings(commonJvmSettings)

// compiles with scala native. XmlParser broken on nativeLink step. will wait for scalameta scala3 migration
lazy val compiler213 = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("compiler213"))
  .dependsOn(utils, syntax, err)
  .settings(
    scala2Common,
    libraryDependencies += ("org.scalameta" %%% "scalameta" % "4.13.5")
      .cross(CrossVersion.for3Use2_13)
      .exclude("org.jline", "jline"),
    // scalap is a dependency of scalameta
    libraryDependencies += ("org.scala-lang" % "scalap" % scala2Version).exclude("org.jline", "jline")
  )
  .jvmSettings(commonJvmSettings, scala2JVM)
lazy val compiler = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("compiler"))
  .dependsOn(semantics)
  .jvmConfigure(_.dependsOn(compiler213.jvm))
  .jsConfigure(_.dependsOn(compiler213.js))
  .settings(
    name := "compiler",
    commonSettings
  )
  .jvmSettings(commonJvmSettings)

val sootupVersion = "1.3.0"
lazy val platform = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("platform"))
  .dependsOn(compatibility, compiler, semantics, core)
  .jsConfigure(
    _.dependsOn(jsTypings.js)
  )
  .settings(
    name := "platform",
    commonSettings
  )
  .jvmSettings(
    commonJvmSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scalap" % scala2Version exclude ("org.jline", "jline"), // dependency of semanticdb-shared
      "org.scalameta" %% "semanticdb-shared" % "4.13.5" cross CrossVersion.for3Use2_13 exclude ("com.lihaoyi", "sourcecode_2.13") exclude (
        "org.jline",
        "jline"
      ),
      "org.scala-lang.modules" % "scala-asm" % "9.8.0-scala-1" % Provided, // Provided by scala-compiler-2
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
      // suppose to support normal jvm https://github.com/oracle/graaljs/blob/master/docs/user/RunOnJDK.md
      // https://www.graalvm.org/latest/reference-manual/native-image/guides/build-polyglot-native-executable/
      // "org.graalvm.polyglot" % "polyglot" % graalvmVersion,
      // "org.graalvm.polyglot" % "js" % graalvmVersion
      "org.bytedeco" % "llvm-platform" % "19.1.3-1.5.11" % Provided
    )
  )

lazy val optional = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("optional"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.bytedeco" % "llvm-platform" % "19.1.3-1.5.11"
    )
  )
  .jvmSettings(jvmScala3Settings, commonJvmSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .dependsOn(utils, reader, syntax, pretty, semantics)
  .settings(
    name := "core",
    assembly / assemblyOutputPath := file("target") / "chester-core.jar",
    commonSettings
    // cpsSettings,
  )
  .jvmSettings(commonJvmSettings)

lazy val jsTypings = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("js-typings"))
  .jsEnablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    commonVendorSettings
  )
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "vscode-languageserver" -> "9.0.1",
      "vscode-languageserver-textdocument" -> "1.0.12",
      "vscode-languageserver-protocol" -> "3.17.5"
    ),
    Compile / npmDependencies ++= Seq(
      "@types/node" -> "22.7.0", // the latest version broken
      "@xterm/xterm" -> "5.5.0",
      "@types/react" -> "19.0.0", // the latest version broken
      "@types/react-dom" -> "19.0.0", // the latest version broken
      "next" -> "11.1.4", // next.js 14/12 breaks scalablytyped
      "xterm-readline" -> "1.1.2",
      "xterm-pty" -> "0.11.1",
      "ts-morph" -> "25.0.1"
    )
  )

lazy val compatibility = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("compatibility"))
  .dependsOn(utils, err)
  .jsConfigure(
    _.dependsOn(jsTypings.js)
  )
  .settings(
    commonSettings
  )
  .jvmSettings(
    commonJvmSettings
  )
  .nativeSettings(
    scalacOptions ++= (if (supportNativeBuildForTermux)
                         Seq(
                           "-Xmacro-settings:com.eed3si9n.ifdef.declare:scalaNativeNoMultithread"
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

val jlineVersion = "3.30.1"
lazy val cli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("cli"))
  .jvmEnablePlugins(NativeImagePlugin)
  .enablePlugins(BuildInfoPlugin) // Enable the BuildInfoPlugin
  .dependsOn(compatibility, platform, compiler)
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
    jvmScala3Settings,
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
  .jsConfigure(project => project.enablePlugins(ScalaJSBundlerPlugin))
  .jsSettings(
    Compile / npmDependencies ++= Seq(
      "ts-morph" -> "25.0.1"
    ),
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
  .dependsOn(compatibility, core)
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
  .dependsOn(buildProtocol, semantics, core)
  .settings(
    libraryDependencies ++= Seq(
    ),
    Compile / mainClass := Some("chester.lsp.Main"),
    libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0-SNAPSHOT",
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
    jvmScala3Settings,
    graalvmSettings
  )

lazy val buildProtocol = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("build-protocol"))
  .dependsOn(compatibility)
  .settings(
    name := "build-protocol",
    commonSettings,
    libraryDependencies ++= Seq(
      // "ch.epfl.scala" %%% "bsp4s" % "2.2.0-M4.TEST" cross (CrossVersion.for3Use2_13) exclude("com.lihaoyi", "sourcecode_2.13") exclude("org.typelevel", "cats-core_2.13") exclude("org.typelevel", "cats-kernel_2.13"),
      // "com.lihaoyi" %%% "sourcecode" % "0.4.3-M1",
      // "org.typelevel" %%% "cats-core" % "2.13.0",
      // "org.typelevel" %%% "cats-kernel" % "2.13.0",
      "org.log4s" %%% "log4s" % "1.10.0",
      "org.slf4j" % "slf4j-api" % "2.0.17",
      "org.slf4j" % "slf4j-simple" % "2.0.17",
      "ch.epfl.scala" % "bsp4j" % "2.2.0-M4.TEST"
    )
  )
  .jvmSettings(commonJvmSettings)

val jgitVersion = "7.2.1.202505142326-r"
lazy val buildTool = crossProject(JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("build-tool"))
  .jvmEnablePlugins(NativeImagePlugin)
  .dependsOn(buildProtocol, semantics, core)
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

lazy val interpreter = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("interpreter"))
  .dependsOn(reader, semantics)
  .settings(commonSettings)
  // https://github.com/b-studios/scala-graal-truffle-example/blob/c2747a6eece156f878c5b934116aaa00a2cd6311/build.sbt
  .settings(
    assembly / assemblyOutputPath := file("target") / "chester-interpreter.jar"
  )
  .jvmSettings(
    javaSemanticdbEnabled := false, // https://github.com/scalameta/metals/issues/3903
    libraryDependencies += "org.jetbrains" % "annotations" % "26.0.2",
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
  .dependsOn(utils, reader, compiler, compiler213, syntax, err, pretty, semantics, platform, lsp, cli, platform, core, interpreter)
  .settings(
    commonSettings
  )

lazy val root = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("project/root"))
  .aggregate(
    allprojects,
    spireNative,
    kiamaCore,
    jsTypings,
    utils,
    reader,
    compiler,
    compiler213,
    syntax,
    err,
    pretty,
    semantics,
    platform,
    core,
    compatibility,
    cli,
    lsp,
    buildProtocol,
    buildTool,
    interpreter,
    js,
    site,
    docs,
    optional,
    jsForPython,
    jsForLua,
    testCommon,
    scalaSTM
  )
  .settings(
    scalaVersion := scala3Version
  )

Global / excludeLintKeys ++= Set[SettingKey[?]](
  cli.jvm / nativeImageJvm,
  cli.jvm / nativeImageVersion,
  cli.js / nativeImageJvm,
  cli.js / nativeImageVersion,
  cli.native / nativeImageJvm,
  cli.native / nativeImageVersion,
  lsp.jvm / nativeImageJvm,
  lsp.jvm / nativeImageVersion
)

// js-for-python integration
lazy val jsForPython = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Full)
  .in(file("js-for-python"))
  .settings(
    commonSettings,
    name := "js-for-python"
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

// Add the jsForLua project
lazy val jsForLua = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Full)
  .in(file("js-for-lua"))
  .settings(
    commonSettings,
    name := "js-for-lua"
  )
  .jsConfigure(_.dependsOn(utils.js))
  .jsSettings(
    scalaJSLinkerConfig ~= {
      // Enable ECMAScript module output
      _.withModuleKind(ModuleKind.ESModule)
        // Use .mjs extension
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    }
  )
