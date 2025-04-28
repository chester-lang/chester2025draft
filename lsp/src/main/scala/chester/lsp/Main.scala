package chester.lsp

import org.eclipse.lsp4j.launch.LSPLauncher
import org.log4s.*
import org.slf4j.simple.SimpleLogger
import chester.i18n.*
import chester.utils.asInt

import java.io.{InputStream, OutputStream}
import java.net.ServerSocket

def enableDebug(): Unit = {
  val _ = System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "Trace")
}

object Main {
  enableDebug()
  private val logger = getLogger
  def main(args: Array[String]): Unit = {
    val port = if (args.nonEmpty) args(0).asInt else 1044
    logger.info(t"Starting Chester Language Server on port $port")
    logger.debug("Debugging enabled")
    logger.trace("Trace enabled")

    val serverSocket = new ServerSocket(port)
    logger.info("Server socket created, waiting for client connection...")

    val clientSocket = serverSocket.accept()
    logger.info(t"Client connected from ${clientSocket.getInetAddress}")

    val in: InputStream = clientSocket.getInputStream
    val out: OutputStream = clientSocket.getOutputStream

    val server = new ChesterLanguageServer()
    logger.info("ChesterLanguageServer instance created")

    val launcher = LSPLauncher.createServerLauncher(server, in, out)
    logger.info("LSP Launcher created")

    val client = launcher.getRemoteProxy
    server.connect(client)
    logger.info("Client connected to the server")

    launcher.startListening()
    logger.info("Server started listening")
  }
}
