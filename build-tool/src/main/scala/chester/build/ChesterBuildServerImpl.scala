package chester.build

import ch.epfl.scala.bsp4j.*
import org.log4s.*
import chester.reader.{ChesterReader, FileNameAndContent}
import chester.tyck.{TyckResult, Tycker}
import chester.i18n.*

import java.util.concurrent.CompletableFuture
import java.util.Collections
import scala.jdk.CollectionConverters.*
import java.nio.file.Paths
import java.nio.file.Files

class ChesterBuildServerImpl extends ChesterBuildServer with BuildServer {

  private val logger = getLogger

  private var client: BuildClient = scala.compiletime.uninitialized

  // Implementing onConnectWithClient
  def onConnectWithClient(client: BuildClient): Unit = {
    this.client = client
    logger.info("Build client connected to the server")
    // Additional setup if needed
  }

  // Implement BuildServer methods

  override def buildInitialize(
      params: InitializeBuildParams
  ): CompletableFuture[InitializeBuildResult] = {
    logger.info(t"Received buildInitialize request with params: $params")

    val capabilities = new BuildServerCapabilities()
    capabilities.setCompileProvider(
      new CompileProvider(Collections.singletonList("chester"))
    )
    capabilities.setTestProvider(
      new TestProvider(Collections.singletonList("chester"))
    )
    capabilities.setRunProvider(
      new RunProvider(Collections.singletonList("chester"))
    )
    // Set other capabilities as needed

    val result = new InitializeBuildResult(
      "Chester Build Server",
      "1.0.0",
      "2.0.0",
      capabilities
    )

    logger.debug(t"Build initialized with capabilities: $capabilities")

    // After initialization, read the package.chester file
    val packageFile = Paths.get("package.chester")
    if (Files.exists(packageFile)) {
      Files.readString(packageFile)
      // Parse the content and store necessary information
    } else {
      logger.error("package.chester file not found.")
      // Handle error appropriately
    }

    CompletableFuture.completedFuture(result)
  }

  override def onBuildInitialized(): Unit =
    logger.info("Build server has been initialized")
    // Additional initialization logic

  override def buildShutdown(): CompletableFuture[Object] = {
    logger.info("Received buildShutdown request")
    CompletableFuture.completedFuture(null)
  }

  override def onBuildExit(): Unit = {
    logger.info("Received onBuildExit notification")
    // Cleanup resources if needed
    System.exit(0)
  }

  override def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] =
    CompletableFuture.supplyAsync { () =>
      val targets = new java.util.ArrayList[BuildTarget]()

      val btCapabilities = new BuildTargetCapabilities()
      btCapabilities.setCanCompile(true)
      btCapabilities.setCanTest(true)
      btCapabilities.setCanRun(false)

      val mainTarget = new BuildTarget(
        new BuildTargetIdentifier("src/main/chester"),
        java.util.Collections.singletonList("chester"),
        java.util.Collections.emptyList(),
        java.util.Collections.emptyList(),
        btCapabilities
      )
      mainTarget.setBaseDirectory("src/main/chester")
      targets.add(mainTarget)

      val testTarget = new BuildTarget(
        new BuildTargetIdentifier("src/test/chester"),
        java.util.Collections.singletonList("chester"),
        java.util.Collections.singletonList(mainTarget.getId.getUri),
        java.util.Collections.emptyList(),
        btCapabilities
      )
      testTarget.setBaseDirectory("src/test/chester")
      targets.add(testTarget)

      new WorkspaceBuildTargetsResult(targets)
    }

  override def buildTargetSources(
      params: SourcesParams
  ): CompletableFuture[SourcesResult] = {
    logger.info(t"Received buildTargetSources request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("buildTargetSources is not implemented yet")
    )
  }

  override def buildTargetInverseSources(
      params: InverseSourcesParams
  ): CompletableFuture[InverseSourcesResult] = {
    logger.info(
      t"Received buildTargetInverseSources request with params: $params"
    )
    CompletableFuture.failedFuture(
      new NotImplementedError(
        "buildTargetInverseSources is not implemented yet"
      )
    )
  }

  override def buildTargetDependencySources(
      params: DependencySourcesParams
  ): CompletableFuture[DependencySourcesResult] = {
    logger.info(
      t"Received buildTargetDependencySources request with params: $params"
    )
    CompletableFuture.failedFuture(
      new NotImplementedError(
        "buildTargetDependencySources is not implemented yet"
      )
    )
  }

  override def buildTargetResources(
      params: ResourcesParams
  ): CompletableFuture[ResourcesResult] = {
    logger.info(t"Received buildTargetResources request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("buildTargetResources is not implemented yet")
    )
  }

  override def buildTargetCompile(
      params: CompileParams
  ): CompletableFuture[CompileResult] =
    CompletableFuture.supplyAsync { () =>
      logger.info(t"Received buildTargetCompile request with params: $params")
      try {
        // Call the type checking and object generation methods
        typeCheckSources()
        generateObjects()

        val result = new CompileResult(StatusCode.OK)
        logger.info("Compilation succeeded.")
        result
      } catch {
        case e: Exception =>
          logger.error(t"Compilation failed. $e")
          val result = new CompileResult(StatusCode.ERROR)
          result
      }
    }

  override def buildTargetTest(
      params: TestParams
  ): CompletableFuture[TestResult] = {
    logger.info(t"Received buildTargetTest request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("buildTargetTest is not implemented yet")
    )
  }

  override def buildTargetRun(
      params: RunParams
  ): CompletableFuture[RunResult] = {
    logger.info(t"Received buildTargetRun request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("buildTargetRun is not implemented yet")
    )
  }

  override def buildTargetCleanCache(
      params: CleanCacheParams
  ): CompletableFuture[CleanCacheResult] = {
    logger.info(t"Received buildTargetCleanCache request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("buildTargetCleanCache is not implemented yet")
    )
  }

  // Implementing missing methods from BuildServer interface

  override def buildTargetDependencyModules(
      params: DependencyModulesParams
  ): CompletableFuture[DependencyModulesResult] = {
    logger.info(
      t"Received buildTargetDependencyModules request with params: $params"
    )
    CompletableFuture.failedFuture(
      new NotImplementedError(
        "buildTargetDependencyModules is not implemented yet"
      )
    )
  }

  override def buildTargetOutputPaths(
      params: OutputPathsParams
  ): CompletableFuture[OutputPathsResult] = {
    logger.info(t"Received buildTargetOutputPaths request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("buildTargetOutputPaths is not implemented yet")
    )
  }

  override def debugSessionStart(
      params: DebugSessionParams
  ): CompletableFuture[DebugSessionAddress] = {
    logger.info(t"Received debugSessionStart request with params: $params")
    CompletableFuture.failedFuture(
      new NotImplementedError("debugSessionStart is not implemented yet")
    )
  }

  override def onRunReadStdin(params: ReadParams): Unit = {
    logger.info(t"Received onRunReadStdin notification with params: $params")
    throw new NotImplementedError("onRunReadStdin is not implemented yet")
  }

  override def workspaceReload(): CompletableFuture[Object] = {
    logger.info("Received workspaceReload request")
    CompletableFuture.failedFuture(
      new NotImplementedError("workspaceReload is not implemented yet")
    )
  }

  // Implement ChesterBuildServer methods

  override def buildTargetChesterOptions(
      params: ChesterOptionsParams
  ): CompletableFuture[ChesterOptionsResult] =
    CompletableFuture.supplyAsync { () =>
      logger.info(
        t"Received buildTargetChesterOptions request with params: $params"
      )

      val items = params.targets.asScala.map { targetId =>
        // Define compiler options, classpath, and class directory for each target
        val options = Seq("-option1", "-option2").asJava
        val classpath = Seq("lib/dependency1", "lib/dependency2").asJava
        val classDirectory = s"out/${targetId.getUri}"

        new ChesterOptionsItem(
          targetId,
          options,
          classpath,
          classDirectory
        )
      }.asJava

      new ChesterOptionsResult(items)
    }

  private def typeCheckSources(): Unit = {
    val sourceDirs = Seq(
      Paths.get("src/main/chester"),
      Paths.get("src/test/chester")
    )

    sourceDirs.foreach { dir =>
      if (Files.exists(dir) && Files.isDirectory(dir)) {
        Files.walk(dir).forEach { path =>
          if (Files.isRegularFile(path) && path.toString.endsWith(".chester")) {
            val content = Files.readString(path)
            ChesterReader
              .parseTopLevel(FileNameAndContent(path.toString, content))
              .fold(
                error =>
                  logger
                    .error(t"Parsing failed for file: $path with error: $error"),
                parsedBlock =>
                  Tycker.check(parsedBlock) match {
                    case TyckResult.Success(_, _, _) =>
                      logger.info(t"Type checking succeeded for file: $path")
                    case TyckResult.Failure(errors, _, _, _) =>
                      logger.error(
                        t"Type checking failed for file: $path with errors: $errors"
                      )
                  }
              )
          }
        }
      } else {
        logger.warn(t"Source directory not found or is not a directory: $dir")
      }
    }
  }

  private def generateObjects(): Unit = ()
}
