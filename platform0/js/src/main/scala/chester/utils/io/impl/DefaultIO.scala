package chester.utils.io.impl

import chester.utils.io.*
import typings.node.bufferMod.global.BufferEncoding
import typings.node.fsMod.MakeDirectoryOptions
import typings.node.{fsMod, fsPromisesMod, osMod, pathMod, processMod, childProcessMod}
import typings.node.childProcessMod.{SpawnSyncOptions, IOType}

import scala.scalajs.js.Thenable.Implicits.*
import java.io.IOException
import scala.concurrent.Future
import scala.scalajs.js
import scala.concurrent.ExecutionContext.Implicits.global
import typings.std.global.fetch
import js.JSConverters.*

import scala.scalajs.js.typedarray.*

implicit object DefaultIO extends IO[Future] {
  // https://stackoverflow.com/questions/75031248/scala-js-convert-uint8array-to-arraybyte/75344498#75344498
  def toScalaArray(input: Uint8Array): Array[Byte] = {
    // Create a view as Int8 on the same underlying data.
    new Int8Array(input.buffer, input.byteOffset, input.length).toArray
  }

  type Path = String

  def pathOps = PathOpsString

  inline override def println(x: String): Future[Unit] =
    Future.successful(Predef.println(x))

  override inline def ask(x: String): String = ???

  inline override def readString(path: String): Future[String] =
    fsPromisesMod.readFile(path, BufferEncoding.utf8)

  // TODO: maybe use https://stackoverflow.com/questions/75031248/scala-js-convert-uint8array-to-arraybyte
  inline override def read(path: String): Future[Array[Byte]] = for {
    buffer <- fsPromisesMod.readFile(path)
  } yield toScalaArray(buffer.asInstanceOf[Uint8Array])

  inline override def writeString(
      path: String,
      content: String,
      append: Boolean = false
  ): Future[Unit] = {
    if (append) {
      fsPromisesMod.appendFile(path, content)
    } else {
      fsPromisesMod.writeFile(path, content)
    }
  }

  // https://stackoverflow.com/questions/76455786/scala-js-how-to-convert-arraybyte-to-blob/76463887#76463887
  inline override def write(path: String, content: Array[Byte]): Future[Unit] =
    fsPromisesMod.writeFile(path, content.toTypedArray)

  inline override def removeWhenExists(path: String): Future[Boolean] =
    fsPromisesMod.unlink(path).map(_ => true).recover { case _: js.JavaScriptException =>
      false
    }

  inline override def workingDir: Future[String] =
    Future.successful(processMod.^.cwd())

  inline override def getHomeDir: Future[String] =
    Future.successful(osMod.homedir())

  inline override def exists(path: String): Future[Boolean] =
    Future.successful(fsMod.existsSync(path))

  inline override def createDirRecursiveIfNotExists(
      path: String
  ): Future[Unit] =
    fsPromisesMod
      .mkdir(path, MakeDirectoryOptions().setRecursive(true))
      .map(_ => ())

  inline override def downloadToFile(url: String, path: String): Future[Unit] =
    for {
      fetched <- fetch(url).toFuture
      read <-
        if fetched.ok then fetched.arrayBuffer().toFuture
        else Future.failed(new IOException(s"Failed to fetch $url"))
      _ <- fsPromisesMod.writeFile(path, new Uint8Array(read))
    } yield ()

  inline override def chmodExecutable(path: String): Future[Unit] =
    fsPromisesMod.chmod(path, "755")

  inline override def getAbsolutePath(path: String): Future[String] =
    Future.successful(pathMod.resolve(path))
  inline override def call(command: Seq[String]): Future[CommandOutput] = {
    val result = childProcessMod.spawnSync(command.head, command.tail.toJSArray, SpawnSyncOptions().setStdio(IOType.inherit))
    val status = result.status match {
      case null      => None
      case s: Double => Some(s.toInt)
    }
    Future.successful(CommandOutput(status))
  }

  override def listFiles(path: String): Future[Seq[String]] = {
    fsPromisesMod
      .readdir(path)
      .map(_.toSeq.map(file => pathMod.join(path, file)))
  }

  override def isDirectory(path: String): Future[Boolean] = {
    fsPromisesMod.lstat(path).map(_.isDirectory())
  }
}
