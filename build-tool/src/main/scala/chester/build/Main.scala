package chester.build

import ch.epfl.scala.bsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.log4s._

import java.util.concurrent.Executors

object Main {
  private val logger = getLogger

  def main(args: Array[String]): Unit = {
    logger.info("Starting Chester Build Server")

    val server = new ChesterBuildServerImpl()

    val executorService = Executors.newFixedThreadPool(4)

    val launcher = new Launcher.Builder[BuildClient]()
      .setOutput(System.out)
      .setInput(System.in)
      .setLocalService(server)
      .setRemoteInterface(classOf[BuildClient])
      .setExecutorService(executorService)
      .create()

    val client = launcher.getRemoteProxy
    server.onConnectWithClient(client)
    logger.info("Client connected to the build server")

    val listening = launcher.startListening()
    logger.info("Build server started listening")

    // Wait for the listening to complete (e.g., when the client disconnects)
    listening.get()

    // Shutdown the executor service
    executorService.shutdown()
    logger.info("Build server shut down")
  }
}
