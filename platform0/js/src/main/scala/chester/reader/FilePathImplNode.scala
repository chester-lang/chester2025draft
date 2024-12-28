package chester.reader

import typings.node.fsMod
import typings.node.bufferMod.global.BufferEncoding

implicit object FilePathImplNode extends FilePathImpl {
  override def readContent(fileName: String): Either[ParseError, String] =
    Right(fsMod.readFileSync(fileName, BufferEncoding.utf8))

  override def absolute(fileName: String): String =
    fsMod.realpathSync(fileName, BufferEncoding.utf8)
}
