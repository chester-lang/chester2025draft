package chester.reader

import chester.utils.*
import chester.i18n.*

import java.nio.file.Paths
import scala.util.*

object FilePathImplJVM extends FilePathImpl {
  override def readContent(fileName: String): Either[ParseError, String] =
    Try(readFileFrom(fileName)) match {
      case Success(content) =>
        Right(content)
      case Failure(exception) =>
        Left(
          ParseError(t"Failed to read file: ${exception.getMessage}")
        )
    }

  override def absolute(fileName: String): String =
    Paths.get(fileName).toAbsolutePath.toString
}

given filePathImplJVM: FilePathImpl = FilePathImplJVM
