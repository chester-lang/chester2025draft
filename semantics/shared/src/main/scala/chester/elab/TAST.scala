package chester.elab

import chester.error.*
import chester.syntax.*
import chester.uniqid.*
import upickle.default.*
import scala.language.implicitConversions
import chester.syntax.core.*
import chester.uniqid.ContainsUniqid
import upickle.default as upickle

implicit val ProblemReaderRW: ReadWriter[() => Vector[TyckProblem]] = readwriter[Vector[TyckProblem]].bimap(
  (problems: () => Vector[TyckProblem]) => problems(),
  (problems: Vector[TyckProblem]) => () => problems
)
implicit val BMRW: ReadWriter[BlockTerm | MetaTerm] = readwriter[Term].bimap(
  (term: BlockTerm | MetaTerm) => term,
  (term: Term) =>
    term match {
      case b: BlockTerm => b
      case m: MetaTerm  => m
    }
)

object TAST {
  def termToBlock(ast: Term): BlockTerm | MetaTerm = ast match {
    case b: BlockTerm => b
    case m: MetaTerm  => m
    case x            => BlockTerm(Vector(), x, x.meta)
  }
  def apply(fileName: String, module: ModuleRef, ast: Term, ty: Term, effects: EffectsM, problems: () => Vector[TyckProblem]): TAST = new TAST(
    fileName = fileName,
    module = module,
    ast = termToBlock(ast),
    ty = ty,
    effects = effects,
    problems = problems
  )
}

// Typed Abstract Syntax Trees
// files
// TODO: handle SourcePos for performance and file size, especially avoid duplicated SourceOffset
case class TAST(
    fileName: String,
    module: ModuleRef,
    ast: BlockTerm | MetaTerm,
    ty: Term,
    effects: EffectsM,
    problems: () => Vector[TyckProblem]
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
}
