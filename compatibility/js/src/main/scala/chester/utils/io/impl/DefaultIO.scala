package chester.utils.io.impl

import chester.utils.io.*
import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsMod.MakeDirectoryOptions
import typings.node.{childProcessMod, fsMod, fsPromisesMod, osMod, pathMod, processMod}
import typings.node.childProcessMod.{IOType, SpawnSyncOptions}
import typings.std.global.fetch
import chester.i18n.*

import scala.scalajs.js.Thenable.Implicits.*
import java.io.IOException
import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.*
import scala.scalajs.js.JSConverters.*

given DefaultIO: IO[Future] {
  // https://stackoverflow.com/questions/75031248/scala-js-convert-uint8array-to-arraybyte/75344498#75344498
  def toScalaArray(input: Uint8Array): Array[Byte] =
    // Create a view as Int8 on the same underlying data.
    new Int8Array(input.buffer, input.byteOffset, input.length).toArray

  type Path = String

  def pathOps = PathOpsString

  override inline def println(x: String): Future[Unit] =
    Future.successful(Predef.println(x))

  override inline def ask(x: String): Future[String] = ???

  override inline def readString(path: String): Future[String] =
    fsPromisesMod.readFile(path, BufferEncoding.utf8)

  // TODO: maybe use https://stackoverflow.com/questions/75031248/scala-js-convert-uint8array-to-arraybyte
  override inline def read(path: String): Future[Array[Byte]] = for {
    buffer <- fsPromisesMod.readFile(path)
  } yield toScalaArray(buffer.asInstanceOf[Uint8Array])

  override inline def writeString(
      path: String,
      content: String,
      append: Boolean = false
  ): Future[Unit] =
    if (append) {
      fsPromisesMod.appendFile(path, content)
    } else {
      fsPromisesMod.writeFile(path, content)
    }

  // https://stackoverflow.com/questions/76455786/scala-js-how-to-convert-arraybyte-to-blob/76463887#76463887
  override inline def write(path: String, content: Array[Byte]): Future[Unit] =
    fsPromisesMod.writeFile(path, content.toTypedArray)

  override inline def removeWhenExists(path: String): Future[Boolean] =
    fsPromisesMod.unlink(path).map(_ => true).recover { case _: js.JavaScriptException =>
      false
    }

  override inline def workingDir: Future[String] =
    Future.successful(processMod.^.cwd())

  override inline def getHomeDir: Future[String] =
    Future.successful(osMod.homedir())

  override inline def exists(path: String): Future[Boolean] =
    Future.successful(fsMod.existsSync(path))

  override inline def createDirRecursiveIfNotExists(
      path: String
  ): Future[Unit] =
    fsPromisesMod
      .mkdir(path, MakeDirectoryOptions().setRecursive(true))
      .map(_ => ())

  override inline def downloadToFile(url: String, path: String): Future[Unit] =
    for {
      fetched <- fetch(url).toFuture
      read <-
        if fetched.ok then fetched.arrayBuffer().toFuture
        else Future.failed(new IOException(t"Failed to fetch $url"))
      _ <- fsPromisesMod.writeFile(path, new Uint8Array(read))
    } yield ()

  override inline def chmodExecutable(path: String): Future[Unit] =
    fsPromisesMod.chmod(path, "755")

  override inline def getAbsolutePath(path: String): Future[String] =
    Future.successful(pathMod.resolve(path))
  override inline def call(command: Seq[String]): Future[CommandOutput] = {
    val result = childProcessMod.spawnSync(command.head, command.tail.toJSArray, SpawnSyncOptions().setStdio(IOType.inherit))
    val status = result.status match {
      case null      => None
      case s: Double => Some(s.toInt)
    }
    Future.successful(CommandOutput(status))
  }

  override def listFiles(path: String): Future[Seq[String]] =
    fsPromisesMod
      .readdir(path)
      .map(_.toSeq.map(file => pathMod.join(path, file)))

  override def isDirectory(path: String): Future[Boolean] =
    fsPromisesMod.lstat(path).map(_.isDirectory())
}
