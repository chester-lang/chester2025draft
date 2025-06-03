package chester.elab

import chester.syntax.core.{BlockTerm, Effects, Term}
import upickle.default.*
import upickle.default as upickle
import chester.i18n.*
import chester.error.*
import chester.syntax.*
import chester.uniqid.*
import chester.syntax.core.*
import chester.syntax.core.orm.{OrM, given}
import chester.uniqid.ContainsUniqid

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

implicit val ProblemReaderRW: ReadWriter[() => Vector[TyckProblem]] = readwriter[Vector[TyckProblem]].bimap(
  (problems: () => Vector[TyckProblem]) => problems(),
  (problems: Vector[TyckProblem]) => () => problems
)

object TAST {
  def termToBlock(ast: Term): OrM[BlockTerm] = ast match {
    case b: BlockTerm => b
    case m: MetaTerm  => m
    case x            => BlockTerm(Vector(), x, x.meta)
  }
  def termToBlockNoMeta(ast: Term): BlockTerm = ast match {
    case b: BlockTerm => b
    case m: MetaTerm  => throw new IllegalArgumentException(s"Expected BlockTerm, but got MetaTerm: $m")
    case x            => BlockTerm(Vector(), x, x.meta)
  }
  def apply(fileName: String, module: ModuleRef, ast: Term, ty: Term, effects: EffectsM, problems: () => Vector[TyckProblem]): TAST = new TAST(
    fileName = fileName,
    module = module,
    ast = termToBlock(ast),
    ty = ty,
    effects = effects,
    getProblems = problems
  )
}

// Typed Abstract Syntax Trees
// files
// TODO: handle SourcePos for performance and file size, especially avoid duplicated SourceOffset
open case class TAST(
    fileName: String,
    module: ModuleRef,
    ast: OrM[BlockTerm],
    ty: Term,
    effects: EffectsM,
    getProblems: () => Vector[TyckProblem]
) extends ContainsUniqid derives ReadWriter {
  override def collectU(collector: UCollector): Unit = {
    ast.collectU(collector)
    ty.collectU(collector)
    effects.collectU(collector)
  }

  override def replaceU(reranger: UReplacer): TAST =
    copy(
      ast = ast.replaceU(reranger).asInstanceOf[BlockTerm],
      ty = ty.replaceU(reranger),
      effects = effects.replaceU(reranger).asInstanceOf[Effects]
    )

  def writeBinary: Array[Byte] = upickle.writeBinary[TAST](this)

  def readBinary(bytes: Array[Byte]): TAST = upickle.readBinary[TAST](bytes)

  def writeString: String = upickle.write[TAST](this)

  def readString(str: String): TAST = upickle.read[TAST](str)

  def zonked(ast: BlockTerm, effects: Effects): ZonkedTAST =
    ZonkedTAST(
      fileName = fileName,
      module = module,
      ast = ast,
      ty = ty,
      effects = effects,
      problems = getProblems()
    )
}

class ZonkedTAST(
    fileName: String,
    module: ModuleRef,
    override val ast: BlockTerm,
    ty: Term,
    override val effects: Effects,
    val problems: Vector[TyckProblem]
) extends TAST(fileName = fileName, module = module, ast = ast, ty = ty, effects = effects, getProblems = () => problems) {}

case class LoadedModules(map: HashMap[ModuleRef, Vector[TAST]] = HashMap()) extends AnyVal {
  def add(tast: TAST): LoadedModules =
    if (
      map.contains(tast.module) && map
        .apply(tast.module)
        .exists(_.fileName == tast.fileName)
    ) {
      throw new IllegalArgumentException(
        t"Module ${tast.module} already loaded from file ${tast.fileName}"
      )
    } else {
      val newTASTs = map.getOrElse(tast.module, Vector()) :+ tast
      LoadedModules(map.updated(tast.module, newTASTs))
    }
}

object LoadedModules {
  val Empty: LoadedModules = LoadedModules()
}
