package chester.syntax

import chester.syntax.*
import chester.syntax.core.{BlockTerm, Effects, Term}
import chester.tyck.SeverityMap
import chester.uniqid.*
import chester.utils.*
import upickle.default.*
import upickle.default as upickle

import scala.collection.immutable.HashMap

object TASTPackage {
  onNativeImageBuildTime {
    // it will be lazy val in the JVM bytecode, if we are building a native image, it will be calculated at build time.
    readwriter[TAST]
  }

  // Typed Abstract Syntax Trees
  // files
  // TODO: handle SourcePos for performance and file size, especially avoid duplicated SourceOffset
  case class TAST(
      fileName: String,
      module: ModuleRef,
      ast: BlockTerm,
      ty: Term,
      effects: Effects,
      problems: SeverityMap
  ) extends ContainsUniqId derives ReadWriter {
    override def collectU(collector: UCollector): Unit = {
      ast.collectU(collector)
      ty.collectU(collector)
      effects.collectU(collector)
    }

    override def replaceU(reranger: UReplacer): TAST = {
      copy(
        ast = ast.replaceU(reranger).asInstanceOf[BlockTerm],
        ty = ty.replaceU(reranger),
        effects = effects.replaceU(reranger).asInstanceOf[Effects]
      )
    }

    def writeBinary: Array[Byte] = upickle.writeBinary[TAST](this)

    def readBinary(bytes: Array[Byte]): TAST = upickle.readBinary[TAST](bytes)

    def writeString: String = upickle.write[TAST](this)

    def readString(str: String): TAST = upickle.read[TAST](str)
  }

  case class LoadedModules(map: HashMap[ModuleRef, Vector[TAST]] = HashMap()) extends AnyVal {
    def add(tast: TAST): LoadedModules = {
      if (
        map.contains(tast.module) && map
          .apply(tast.module)
          .exists(_.fileName == tast.fileName)
      ) {
        throw new IllegalArgumentException(
          s"Module ${tast.module} already loaded from file ${tast.fileName}"
        )
      } else {
        val newTASTs = map.getOrElse(tast.module, Vector()) :+ tast
        LoadedModules(map.updated(tast.module, newTASTs))
      }
    }
  }

  object LoadedModules {
    val Empty: LoadedModules = LoadedModules()
  }

}

export TASTPackage.*
