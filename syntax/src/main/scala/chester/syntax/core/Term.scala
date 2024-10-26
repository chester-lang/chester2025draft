// TODO: More correctly implement toDoc
package chester.syntax.core

import cats.data.*
import chester.doc.*
import chester.doc.const.{ColorProfile, Docs}
import chester.error.*
import chester.error.ProblemUpickle.*
import chester.syntax.core.orm.*
import chester.syntax.*
import chester.utils.{*, given}
import chester.utils.doc.*
import chester.utils.impls.*
import spire.math.{Rational, Trilean}
import spire.math.Trilean.*
import upickle.default.*
import chester.uniqid.*

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

case class TermMeta(sourcePos: SourcePos) derives ReadWriter

type OptionTermMeta = Option[TermMeta]

case class CallingArgTerm(
    value: Term,
    ty: Term,
    name: Option[Name] = None,
    vararg: Boolean = false,
    meta: OptionTermMeta
) extends WHNF derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

  override type ThisTree = CallingArgTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): CallingArgTerm =
    copy(value = f(value), ty = f(ty))
}

case class Calling(
    args: Vector[CallingArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta
) extends WHNF derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  override type ThisTree = Calling
  def descent(f: Term => Term, g: TreeMap[Term]): Calling = copy(args = args.map(g))
}

case class FCallTerm(
    f: Term,
    args: Vector[Calling],
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = FCallTerm

  override def toDoc(using options: PrettierOptions): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

  override def descent(op: Term => Term, g: TreeMap[Term]): FCallTerm = thisOr(
    copy(f = op(f), args = args.map(g))
  )
}

object FCallTerm {}

sealed trait Pat extends ToDoc derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc = toString

  def descent(patOp: Pat => Pat, termOp: Term => Term): Pat = this

  def thisOr(x: Pat): this.type = reuse(this, x.asInstanceOf[this.type])
}

case class Bind(bind: LocalV, ty: Term, meta: OptionTermMeta) extends Pat {
  override def descent(patOp: Pat => Pat, termOp: Term => Term): Pat = thisOr(
    copy(ty = termOp(ty))
  )
}

object Bind {
  @deprecated("meta")
  def from(bind: LocalV): Bind = Bind(bind, bind.ty, None)
}

/** more abstract Term. sealed trait *T corresponds to sealed trait in Term; trait *C corresponds to case class in Term */
trait TermT[Rec <: TermT[Rec]] extends Tree[Rec] {
  def meta: OptionTermMeta
  def whnf: Trilean
  def toTerm: Term = {
    assert(this.isInstanceOf[Term], "forgot to implement toTerm?")
    this.asInstanceOf[Term]
  }
  def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)
}

type AnyTerm = TermT[?]

sealed trait Term extends ToDoc with TermT[Term] with ContainsUniqid derives ReadWriter {
  type ThisTree <: Term
  def meta: OptionTermMeta

  override def toDoc(using options: PrettierOptions): Doc = toString

  def whnf: Trilean

  def descent(f: Term => Term, g: TreeMap[Term]): Term

  def mapFlatten[B](f: Term => Seq[B]): Vector[B] = {
    var result = Vector.empty[B]
    inspectRecursive { term =>
      result ++= f(term)
    }
    result
  }

  def doElevate(level: IntegerTerm): Term = descent(_.doElevate(level))

  final def elevate(level: IntegerTerm): Term = {
    require(level.value >= 0)
    if (level.value == 0) this else doElevate(level)
  }

  // TODO: optimize
  final def substitute[A <: TermWithUniqid](mapping: Seq[(A, Term)]): Term = {
    mapping.foldLeft(this) { case (acc, (from, to)) =>
      acc.substitute(from, to)
    }
  }

  final def substitute(from: TermWithUniqid, to: Term): Term = {
    if (from == to) return this
    if (
      to match {
        case to: TermWithUniqid => from.uniqId == to.uniqId
        case _                  => false
      }
    ) return this
    descentRecursive {
      case x: TermWithUniqid if x.uniqId == from.uniqId => to
      case x                                            => x
    }
  }

  def collectMeta: Vector[MetaTerm] = {
    this match {
      case term: MetaTerm => return Vector(term)
      case _              =>
    }
    var result = Vector.empty[MetaTerm]
    inspect { x => result ++= x.collectMeta }
    result
  }

  def replaceMeta(f: MetaTerm => Term): Term = thisOr {
    this match {
      case term: MetaTerm => f(term)
      case _ =>
        descent2(new TreeMap[Term] {
          def use[T <: Term](x: T): x.ThisTree = x.replaceMeta(f).asInstanceOf[x.ThisTree]
        })
    }
  }

  final override def collectU(collector: UCollector): Unit = inspectRecursive {
    case x: TermWithUniqid => collector(x.uniqId)
    case _                 =>
  }

  final override def replaceU(reranger: UReplacer): Term = descentRecursive {
    case x: TermWithUniqid => x.switchUniqId(reranger)
    case x                 => x
  }
}

sealed trait WHNFT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WHNFT[Rec]
  override def whnf: Trilean = True
}

sealed trait WHNF extends Term with WHNFT[Term] derives ReadWriter {
  override type ThisTree <: WHNF
}

sealed trait UnevalT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: UnevalT[Rec]
  override def whnf: Trilean = False
}

sealed trait Uneval extends Term with UnevalT[Term] derives ReadWriter {
  override type ThisTree <: Uneval
}
sealed trait SpecialTermT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: SpecialTermT[Rec]
  override def whnf: Trilean = Unknown
}

sealed trait SpecialTerm extends Term with SpecialTermT[Term] derives ReadWriter {
  override type ThisTree <: SpecialTerm
}
sealed trait TermWithUniqidT[Rec <: TermT[Rec]] extends TermT[Rec] with HasUniqid {
  override type ThisTree <: TermWithUniqidT[Rec]
  override def uniqId: UniqidOf[Rec]
}

sealed trait TermWithUniqid extends Term with TermWithUniqidT[Term] derives ReadWriter {
  override type ThisTree <: TermWithUniqid
  override def uniqId: UniqidOf[Term]
  def switchUniqId(r: UReplacer): TermWithUniqid
}

sealed trait EffectsMT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: EffectsMT[Rec]
}

sealed trait EffectsM extends Term with EffectsMT[Term] derives ReadWriter {
  override type ThisTree <: EffectsM
}

trait MetaTermC[Rec <: TermT[Rec]] extends TermT[Rec] with EffectsMT[Rec] with SpecialTermT[Rec] {
  override type ThisTree <: MetaTermC[Rec]
  def impl: HoldNotReadable[?]
  override def toTerm: MetaTerm = MetaTerm(impl, meta)
}

case class MetaTerm(impl: HoldNotReadable[?], meta: OptionTermMeta) extends Term with MetaTermC[Term] with EffectsM with SpecialTerm {
  override type ThisTree = MetaTerm
  def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.group("Meta" <> Doc.text(impl.toString))

  override def descent(f: Term => Term, g: TreeMap[Term]): MetaTerm = this
}

object MetaTerm {
  @deprecated("meta")
  def from[T](x: T): MetaTerm = MetaTerm(HoldNotReadable(x), meta = None)
}

trait ListTermC[Rec <: TermT[Rec]] extends TermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: ListTermC[Rec]
  def terms: Vector[Rec]
  override def toTerm: ListTerm = ListTerm(terms.map(_.toTerm), meta)
}

case class ListTerm(terms: Vector[Term], meta: OptionTermMeta) extends Term with ListTermC[Term] with WHNF derives ReadWriter {
  override final type ThisTree = ListTerm
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(terms)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(terms = terms.map(f)))
}

sealed trait TypeTermT[Rec <: TermT[Rec]] extends TermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: TypeTermT[Rec]
}

sealed trait TypeTerm extends Term with TypeTermT[Term] with WHNF derives ReadWriter {
  override type ThisTree <: TypeTerm
}

sealed trait SortT[Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: SortT[Rec]
  def level: TermT[Rec]
}

sealed trait Sort extends TypeTerm with SortT[Term] derives ReadWriter {
  override type ThisTree <: Sort
  def level: Term
}

sealed trait TypeT[Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: TypeT[Rec]
  def level: Rec
}

case class Type(level: Term, meta: OptionTermMeta) extends Sort with TypeT[Term] {
  override type ThisTree = Type
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(Vector(level))

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))
}

trait LevelTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: LevelTypeC[Rec]
}

case class LevelType(meta: OptionTermMeta) extends TypeTerm with WithType with LevelTypeC[Term] {
  override type ThisTree = LevelType
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("LevelType")

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

  override def ty: Term = Type0
}

sealed trait LevelT[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: LevelT[Rec]
}

sealed trait Level extends Term with LevelT[Term] with WHNF derives ReadWriter {
  type ThisTree <: Level
}

trait LevelFiniteC[Rec <: TermT[Rec]] extends LevelT[Rec] {
  override type ThisTree <: LevelFiniteC[Rec]
  def n: Rec
  override def toTerm: LevelFinite = LevelFinite(n.toTerm, meta)
}

case class LevelFinite(n: Term, meta: OptionTermMeta) extends Level with LevelFiniteC[Term] {
  override type ThisTree = LevelFinite
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Level(") <> n.toDoc <> Doc.text(")")

  override def descent(f: Term => Term, g: TreeMap[Term]): LevelFinite =
    thisOr(copy(n = f(n)))
}

trait LevelUnrestrictedC[Rec <: TermT[Rec]] extends LevelT[Rec] {
  override type ThisTree <: LevelUnrestrictedC[Rec]
  override def toTerm: LevelUnrestricted = LevelUnrestricted(meta)
}

case class LevelUnrestricted(meta: OptionTermMeta) extends Level with LevelUnrestrictedC[Term] {
  override type ThisTree = LevelUnrestricted
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Levelω")

  override def descent(f: Term => Term, g: TreeMap[Term]): LevelUnrestricted = this
}

// Define Level0 using LevelFinite
val Level0 = LevelFinite(IntegerTerm(0, meta = None), meta = None)

val Type0 = Type(Level0, meta = None)

// Referencing Setω in Agda
val Typeω = Type(LevelUnrestricted(None), meta = None)

enum Usage derives ReadWriter {
  case None, Linear, Unrestricted
}

trait PropC[Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: PropC[Rec]
  def level: Rec
  override def toTerm: Prop = Prop(level.toTerm, meta)
}

case class Prop(level: Term, meta: OptionTermMeta) extends Sort with PropC[Term] {
  override type ThisTree = Prop
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Prop" <> Docs.`(`, Docs.`)`)(Vector(level))
}

trait FTypeC[Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: FTypeC[Rec]
  def level: Rec
  override def toTerm: FType = FType(level.toTerm, meta)
}

// fibrant types
case class FType(level: Term, meta: OptionTermMeta) extends Sort with FTypeC[Term] {
  override type ThisTree = FType
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
}

trait LiteralTermT[Rec <: TermT[Rec]] extends TermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: LiteralTermT[Rec]
}

sealed trait LiteralTerm extends Term with LiteralTermT[Term] with WHNF derives ReadWriter {
  override type ThisTree <: LiteralTerm
}

sealed trait AbstractIntTermT[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: AbstractIntTermT[Rec]
}

sealed trait AbstractIntTerm extends LiteralTerm with AbstractIntTermT[Term] derives ReadWriter {
  override type ThisTree <: AbstractIntTerm
}

trait IntTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] with AbstractIntTermT[Rec] {
  override type ThisTree <: IntTermC[Rec]
  def value: Int
  override def toTerm: IntTerm = IntTerm(value, meta)
}

case class IntTerm(value: Int, meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntTermC[Term] derives ReadWriter {
  override type ThisTree = IntTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait IntegerTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] with AbstractIntTermT[Rec] {
  override type ThisTree <: IntegerTermC[Rec]
  def value: BigInt
  override def toTerm: IntegerTerm = IntegerTerm(value, meta)
}

case class IntegerTerm(value: BigInt, meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntegerTermC[Term] derives ReadWriter {
  override type ThisTree = IntegerTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

object AbstractIntTerm {
  def from(value: BigInt, meta: OptionTermMeta): AbstractIntTerm =
    if (value.isValidInt) IntTerm(value.toInt, meta)
    else IntegerTerm(value, meta)

  def unapply(term: Term): Option[BigInt] = term match {
    case IntTerm(value, _)     => Some(BigInt(value))
    case IntegerTerm(value, _) => Some(value)
    case _                     => None
  }
}

object NaturalTerm {

  @deprecated("meta")
  def apply(value: BigInt): AbstractIntTerm = AbstractIntTerm.from(value, meta = None)
}

sealed trait WithTypeT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WithTypeT[Rec]
  def ty: Rec
}

sealed trait WithType extends Term with WithTypeT[Term] derives ReadWriter {
  override type ThisTree <: WithType
  def ty: Term
}

trait IntegerTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: IntegerTypeC[Rec]
  override def toTerm: IntegerType = IntegerType(meta)
}

case class IntegerType(meta: OptionTermMeta) extends TypeTerm with WithType with IntegerTypeC[Term] derives ReadWriter {
  override type ThisTree = IntegerType
  override def descent(f: Term => Term, g: TreeMap[Term]): IntegerType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Integer", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait IntTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: IntTypeC[Rec]
  override def toTerm: IntType = IntType(meta)
}

// int of 64 bits or more
case class IntType(meta: OptionTermMeta) extends TypeTerm with WithType with IntTypeC[Term] derives ReadWriter {
  override type ThisTree = IntType
  override def descent(f: Term => Term, g: TreeMap[Term]): IntType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Int", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait UIntTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: UIntTypeC[Rec]
  override def toTerm: UIntType = UIntType(meta)
}

// unsigned int of 64 bits or more
case class UIntType(meta: OptionTermMeta) extends TypeTerm with WithType with UIntTypeC[Term] derives ReadWriter {
  override type ThisTree = UIntType
  override def descent(f: Term => Term, g: TreeMap[Term]): UIntType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("UInt", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait NaturalTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: NaturalTypeC[Rec]
  override def toTerm: NaturalType = NaturalType(meta)
}

case class NaturalType(meta: OptionTermMeta) extends TypeTerm with WithType with NaturalTypeC[Term] derives ReadWriter {
  override type ThisTree = NaturalType
  override def descent(f: Term => Term, g: TreeMap[Term]): NaturalType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait RationalTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: RationalTermC[Rec]
  def value: Rational
  override def toTerm: RationalTerm = RationalTerm(value, meta)
}

case class RationalTerm(value: Rational, meta: OptionTermMeta) extends LiteralTerm with RationalTermC[Term] derives ReadWriter {
  override type ThisTree = RationalTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): RationalTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait BooleanTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: BooleanTermC[Rec]
  def value: Boolean
  override def toTerm: BooleanTerm = BooleanTerm(value, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = this
}

case class BooleanTerm(value: Boolean, meta: OptionTermMeta) extends LiteralTerm with BooleanTermC[Term] derives ReadWriter {
  override type ThisTree = BooleanTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): BooleanTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait BooleanTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: BooleanTypeC[Rec]
  override def toTerm: BooleanType = BooleanType(meta)
}

case class BooleanType(meta: OptionTermMeta) extends TypeTerm with WithType with BooleanTypeC[Term] derives ReadWriter {
  override type ThisTree = BooleanType
  override def descent(f: Term => Term, g: TreeMap[Term]): BooleanType = this

  override def ty: Term = Type0
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Boolean", ColorProfile.typeColor)
}

trait StringTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: StringTermC[Rec]
  def value: String
  override def toTerm: StringTerm = StringTerm(value, meta)
}

case class StringTerm(value: String, meta: OptionTermMeta) extends LiteralTerm with StringTermC[Term] derives ReadWriter {
  override type ThisTree = StringTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): StringTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
}

trait SymbolTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: SymbolTermC[Rec]
  def value: String
  override def toTerm: SymbolTerm = SymbolTerm(value, meta)
}

case class SymbolTerm(value: String, meta: OptionTermMeta) extends LiteralTerm with SymbolTermC[Term] derives ReadWriter {
  override type ThisTree = SymbolTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): SymbolTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("'" + value, ColorProfile.literalColor)
}

trait RationalTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: RationalTypeC[Rec]
  override def toTerm: RationalType = RationalType(meta)
}

case class RationalType(meta: OptionTermMeta) extends TypeTerm with WithType with RationalTypeC[Term] derives ReadWriter {
  override type ThisTree = RationalType
  override def descent(f: Term => Term, g: TreeMap[Term]): RationalType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Rational", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait FloatTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: FloatTypeC[Rec]
  override def toTerm: FloatType = FloatType(meta)
}

// float of 32 bits or more
case class FloatType(meta: OptionTermMeta) extends TypeTerm with WithType with FloatTypeC[Term] derives ReadWriter {
  override type ThisTree = FloatType
  override def descent(f: Term => Term, g: TreeMap[Term]): FloatType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Float", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait StringTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: StringTypeC[Rec]
  override def toTerm: StringType = StringType(meta)
}

case class StringType(meta: OptionTermMeta) extends TypeTerm with WithType with StringTypeC[Term] derives ReadWriter {
  override type ThisTree = StringType
  override def descent(f: Term => Term, g: TreeMap[Term]): StringType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("String", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait SymbolTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: SymbolTypeC[Rec]
  override def toTerm: SymbolType = SymbolType(meta)
}

case class SymbolType(meta: OptionTermMeta) extends TypeTerm with WithType with SymbolTypeC[Term] derives ReadWriter {
  override type ThisTree = SymbolType
  override def descent(f: Term => Term, g: TreeMap[Term]): SymbolType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Symbol", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait AnyTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: AnyTypeC[Rec]
  def level: Rec
  override def toTerm: AnyType = AnyType(level.toTerm, meta)
}

case class AnyType(level: Term, meta: OptionTermMeta) extends TypeTerm with WithType with AnyTypeC[Term] derives ReadWriter {
  override type ThisTree = AnyType
  override def descent(f: Term => Term, g: TreeMap[Term]): AnyType = thisOr(
    copy(level = f(level))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Any", ColorProfile.typeColor)

  override def ty: Term = Type(level, meta)
}

def AnyType0 = AnyType(Level0, meta = None)

val AnyType0Debug = AnyType(Level0, meta = None)

case class NothingType(meta: OptionTermMeta) extends TypeTerm with WithType {
  override type ThisTree = NothingType
  override def descent(f: Term => Term, g: TreeMap[Term]): NothingType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Nothing", ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class LiteralType(
    literal: LiteralTerm,
    meta: OptionTermMeta
) extends TypeTerm
    with WithType {
  override type ThisTree = LiteralType
  override def descent(f: Term => Term, g: TreeMap[Term]): LiteralType =
    copy(literal = g(literal).asInstanceOf[IntegerTerm | SymbolTerm | StringTerm | RationalTerm])

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(literal.toString, ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class ArgTerm(
    bind: LocalV,
    ty: Term,
    default: Option[Term] = None,
    vararg: Boolean = false,
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = ArgTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ArgTerm = thisOr(
    copy(bind = g(bind), ty = f(ty), default = default.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
    bind.toDoc <> varargDoc <> Docs.`:` <+> ty.toDoc <> defaultDoc
  }

  def name: Name = bind.name
}

object ArgTerm {

  @deprecated("meta")
  def from(bind: LocalV): ArgTerm = ArgTerm(bind, bind.ty, meta = None)
}

object TelescopeTerm {
  @deprecated("meta")
  def from(x: ArgTerm*): TelescopeTerm = TelescopeTerm(x.toVector, meta = None)
}

case class TelescopeTerm(
    args: Vector[ArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = TelescopeTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): TelescopeTerm = thisOr(
    copy(args = args.map(g))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc =
      args.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    if (implicitly) {
      Docs.`[` <> argsDoc <> Docs.`]`
    } else {
      Docs.`(` <> argsDoc <> Docs.`)`
    }
  }
}

case class Function(ty: FunctionType, body: Term, meta: OptionTermMeta) extends WHNF {
  override type ThisTree = Function
  override def descent(f: Term => Term, g: TreeMap[Term]): Function = thisOr(
    copy(ty = g(ty), body = f(body))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val paramsDoc =
      ty.telescope.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val returnTypeDoc = Docs.`:` <+> ty.resultTy.toDoc
    val effectsDoc = if (ty.effects.nonEmpty) {
      Docs.`/` <+> ty.effects.toDoc
    } else {
      Doc.empty
    }
    val bodyDoc = body.toDoc
    group(paramsDoc <> returnTypeDoc <+> Docs.`=>` <+> bodyDoc <> effectsDoc)
  }
}

@deprecated("not used")
case class MatchingClause() derives ReadWriter {}

case class Matching(
    ty: FunctionType,
    clauses: NonEmptyVector[MatchingClause],
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = Matching
  override def descent(f: Term => Term, g: TreeMap[Term]): Matching = thisOr(
    copy(ty = g(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc = toString // TODO
}

// Note that effect and result can use variables from telescope
case class FunctionType(
    telescope: Vector[TelescopeTerm],
    resultTy: Term,
    effects: EffectsM = NoEffect,
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = FunctionType
  override def descent(f: Term => Term, g: TreeMap[Term]): FunctionType = thisOr(
    copy(telescope = telescope.map(g), resultTy = f(resultTy), effects = g(effects))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val telescopeDoc =
      telescope.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val effectsDoc = if (effects.nonEmpty) {
      Docs.`/` <+> effects.toDoc
    } else {
      Doc.empty
    }
    group(telescopeDoc <+> Docs.`->` <+> resultTy.toDoc <> effectsDoc)
  }
}

object FunctionType {
  @deprecated("meta")
  def apply(telescope: TelescopeTerm, resultTy: Term): FunctionType = {
    new FunctionType(Vector(telescope), resultTy, meta = None)
  }
}

def TyToty: FunctionType = {
  val ty = LocalV("x", Type0, Uniqid.generate[LocalV], None)
  FunctionType(TelescopeTerm.from(ArgTerm.from(ty)), ty)
}

case class ObjectClauseValueTerm(
    key: Term,
    value: Term,
    meta: OptionTermMeta
) extends WHNF derives ReadWriter {
  override type ThisTree = ObjectClauseValueTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectClauseValueTerm = (
    copy(key = f(key), value = f(value))
  )

  override def toDoc(using options: PrettierOptions): Doc = group(
    key <+> Doc.text("=") <+> value
  )
}

case class ObjectTerm(
    clauses: Vector[ObjectClauseValueTerm],
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = ObjectTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectTerm = thisOr(
    copy(clauses = clauses.map(g))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc))
}

// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
case class ObjectType(
    fieldTypes: Vector[ObjectClauseValueTerm],
    exactFields: Boolean = false,
    meta: OptionTermMeta
) extends WHNF {
  override type ThisTree = ObjectType
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectType = thisOr(
    copy(fieldTypes = fieldTypes.map(g))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(
      fieldTypes.map(_.toDoc)
    )
}

sealed trait Builtin extends WHNF derives ReadWriter {
  override type ThisTree <: Builtin
}

case class ListF(meta: OptionTermMeta) extends Builtin {
  override type ThisTree = ListF
  override def descent(f: Term => Term, g: TreeMap[Term]): ListF = this

  override def toDoc(using options: PrettierOptions): Doc = "List"
}

sealed trait Constructed extends WHNF derives ReadWriter {
  type ThisTree <: Constructed
}

case class ListType(ty: Term, meta: OptionTermMeta) extends Constructed with TypeTerm {
  override type ThisTree = ListType
  override def descent(f: Term => Term, g: TreeMap[Term]): ListType = thisOr(copy(ty = f(ty)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("List") <> Docs.`(` <> ty <> Docs.`)`
}

case class Union(xs: NonEmptyVector[Term], meta: OptionTermMeta) extends TypeTerm {
  override type ThisTree = Union
  override def descent(f: Term => Term, g: TreeMap[Term]): Union = thisOr(copy(xs = xs.map(g)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, " | ")(xs)
}

private inline def flatList[T <: Term](
    inline constructor: Vector[Term] => T,
    inline unapply: Term => Option[Vector[Term]],
    inline post: Vector[Term] => Vector[Term] = x => x
)(inline xs: Vector[Term]) = {
  val flattened = post(xs.flatMap { item =>
    unapply(item).getOrElse(Vector(item))
  })
  constructor(flattened)
}

object Union {
  @deprecated("meta")
  // def apply(xs: Vector[Term]): OrType = flatList[OrType]((x => new OrType(x)), { case OrType(x) => Some(x); case _ => None }, _.distinct)(xs)
  def from(xs: Vector[Term]): Term = {
    val flattened = xs
      .flatMap {
        case Union(ys, _) => ys
        case x            => Vector(x)
      }
      .distinct
      .filter(x => !x.isInstanceOf[NothingType])
    if (flattened.size == 1) return flattened.head
    if (flattened.nonEmpty) new Union(flattened.assumeNonEmpty, None)
    else NothingType(None)
  }
}

case class Intersection(xs: NonEmptyVector[Term], meta: OptionTermMeta) extends TypeTerm derives ReadWriter {
  override type ThisTree = Intersection
  override def descent(f: Term => Term, g: TreeMap[Term]): Intersection = thisOr(
    copy(xs = xs.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, " & ")(xs)
}

object Intersection {
  @deprecated("meta")
  // def apply(xs: Vector[Term]): AndType = flatList[AndType]((x => new AndType(x)), { case AndType(x) => Some(x); case _ => None })(xs)
  def from(xs: Vector[Term]): Term = {
    val flattened = xs.flatMap {
      case Intersection(ys, _) => ys
      case x                   => Vector(x)
    }.distinct
    if (flattened.size == 1) return flattened.head
    new Intersection(flattened.assumeNonEmpty, None)
  }
}

/** Effect needs to have reasonable equals and hashcode for simple comparison, whereas they are not requirements for other Terms
  */
sealed trait Effect extends WHNF derives ReadWriter {
  override type ThisTree <: Effect
  override def descent(f: Term => Term, g: TreeMap[Term]): Effect = this
  def name: String

  override def toDoc(using options: PrettierOptions): Doc = Doc.text(name)
}

case class Effects(effects: Map[LocalV, Term] = HashMap.empty, meta: OptionTermMeta) extends WHNF with EffectsM derives ReadWriter {
  override type ThisTree = Effects
  override def descent(f: Term => Term, g: TreeMap[Term]): Effects = thisOr(
    copy(effects = effects.map { case (k, v) => g(k) -> f(v) })
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(effects.map { case (k, v) =>
      k.toDoc <+> Docs.`:` <+> v.toDoc
    })

  def isEmpty: Boolean = (effects eq NoEffect.effects) || effects.isEmpty

  def nonEmpty: Boolean = (effects ne NoEffect.effects) && effects.nonEmpty

  override def collectMeta: Vector[MetaTerm] =
    effects.flatMap((a, b) => a.collectMeta ++ b.collectMeta).toVector

  override def replaceMeta(f: MetaTerm => Term): Effects = copy(effects = effects.map { case (a, b) =>
    a.replaceMeta(f).asInstanceOf[LocalV] -> b.replaceMeta(f)
  })
}

object Effects {
  val Empty: Effects = Effects(HashMap.empty, meta = None)
}

val NoEffect = Effects.Empty

// may raise an exception
case class ExceptionEffect(meta: OptionTermMeta) extends Effect {
  val name = "Exception"
}

// may not terminate
case class DivergeEffect(meta: OptionTermMeta) extends Effect {
  val name = "Diverge"
}

// whatever IO: console, file, network, ...
case class IOEffect(meta: OptionTermMeta) extends Effect {
  val name = "IO"
}

case class STEffect(meta: OptionTermMeta) extends Effect {
  val name = "ST"
}

sealed trait ReferenceCall extends Term with Uneval with TermWithUniqid derives ReadWriter {
  @deprecated("avoid using this")
  def name: Name
  def ty: Term
  override type ThisTree <: ReferenceCall
}

case class LocalV(
    name: Name,
    ty: Term,
    uniqId: UniqidOf[LocalV],
    meta: OptionTermMeta
) extends ReferenceCall derives ReadWriter {
  override type ThisTree = LocalV
  override def descent(f: Term => Term, g: TreeMap[Term]): LocalV = thisOr(
    copy(ty = f(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(name)

  override def switchUniqId(r: UReplacer): TermWithUniqid = copy(uniqId = r(uniqId))
}

case class ToplevelV(
    id: AbsoluteRef,
    ty: Term,
    uniqId: UniqidOf[ToplevelV],
    meta: OptionTermMeta
) extends ReferenceCall {
  override type ThisTree = ToplevelV
  override def descent(f: Term => Term, g: TreeMap[Term]): ToplevelV = thisOr(
    copy(ty = f(ty))
  )

  override def name: Name = id.name

  override def toDoc(using options: PrettierOptions): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )

  override def switchUniqId(r: UReplacer): TermWithUniqid = copy(uniqId = r(uniqId))
}

case class ErrorTerm(problem: Problem, meta: OptionTermMeta) extends SpecialTerm {
  override type ThisTree = ErrorTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ErrorTerm = this

  override def toDoc(using options: PrettierOptions): Doc = problem.toDoc
}

def ErrorType(error: Problem, meta: OptionTermMeta): ErrorTerm =
  ErrorTerm(error, meta)

sealed trait StmtTerm extends WHNF derives ReadWriter {
  override type ThisTree <: StmtTerm
}

case class LetStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta
) extends StmtTerm {
  override type ThisTree = LetStmtTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): LetStmtTerm = thisOr(
    copy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }
}

case class DefStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta
) extends StmtTerm {
  override type ThisTree = DefStmtTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): DefStmtTerm = thisOr(
    copy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }
}

case class ExprStmtTerm(
    expr: Term,
    ty: Term = AnyType0,
    meta: OptionTermMeta
) extends StmtTerm {
  override type ThisTree = ExprStmtTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ExprStmtTerm = thisOr(
    copy(expr = f(expr), ty = f(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc = expr.toDoc
}

case class NonlocalOrLocalReturn(value: Term, meta: OptionTermMeta) extends StmtTerm {
  override type ThisTree = NonlocalOrLocalReturn
  override def descent(f: Term => Term, g: TreeMap[Term]): NonlocalOrLocalReturn = thisOr(
    copy(value = f(value))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("return") <+> value.toDoc
}

case class TupleType(types: Vector[Term], meta: OptionTermMeta) extends TypeTerm {
  override type ThisTree = TupleType
  override def descent(f: Term => Term, g: TreeMap[Term]): TupleType = thisOr(
    copy(types = types.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`, ",")(types)
  }
}

case class TupleTerm(values: Vector[Term], meta: OptionTermMeta) extends WHNF {
  override type ThisTree = TupleTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): TupleTerm = thisOr(
    copy(values = values.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`(`, Docs.`)`, ",")(values)
  }
}

case class BlockTerm(
    statements: Vector[StmtTerm],
    result: Term,
    meta: OptionTermMeta
) extends Uneval derives ReadWriter {
  override type ThisTree = BlockTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): BlockTerm = thisOr(
    copy(
      statements = statements.map(g),
      result = f(result)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ";")(
      (statements.map(_.toDoc) :+ result.toDoc)
    )
  }
}

case class Annotation(
    term: Term,
    ty: Option[Term],
    effects: Option[EffectsM],
    meta: OptionTermMeta
) extends Uneval {
  override type ThisTree = Annotation
  override def descent(f: Term => Term, g: TreeMap[Term]): Annotation = thisOr(
    copy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(g)
    )
  )

  require(ty.nonEmpty || effects.nonEmpty)

  override def toDoc(using options: PrettierOptions): Doc = {
    val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
    val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
    term.toDoc <> tyDoc <> effectsDoc
  }
}

def UnitType(meta: OptionTermMeta) =
  TupleType(Vector.empty, meta = meta)
object UnitTerm {
  def unapply(x: Any): Option[OptionTermMeta] = x match {
    case TupleTerm(Vector(), meta) => Some(meta)
    case _                         => None
  }
  def apply(meta: OptionTermMeta): TupleTerm =
    TupleTerm(Vector.empty, meta = meta)

}

case class FieldTerm(
    name: Name,
    ty: Term,
    meta: OptionTermMeta
) extends WHNF derives ReadWriter {
  override type ThisTree = FieldTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): FieldTerm = thisOr(
    copy(ty = f(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(name) <> Doc.text(": ") <> ty.toDoc
}

sealed trait TypeDefinition extends StmtTerm with TermWithUniqid derives ReadWriter {
  def name: Name
  def uniqId: UniqidOf[TypeDefinition]

  override def switchUniqId(r: UReplacer): TypeDefinition
}

case class RecordStmtTerm(
    name: Name,
    uniqId: UniqidOf[RecordStmtTerm] = Uniqid.generate[RecordStmtTerm],
    fields: Vector[FieldTerm],
    body: Option[BlockTerm],
    meta: OptionTermMeta
) extends TypeDefinition {
  override type ThisTree = RecordStmtTerm
  override def switchUniqId(r: UReplacer): RecordStmtTerm = copy(uniqId = r(uniqId))
  override def descent(f: Term => Term, g: TreeMap[Term]): RecordStmtTerm = thisOr(
    copy(
      fields = fields.map(g),
      body = body.map(g)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val fieldsDoc = fields.map(_.toDoc).reduceOption(_ <> Doc.text(", ") <> _).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).reduceOption(_ </> _).getOrElse(Doc.empty)

    group(
      Doc.text("record ") <> Doc.text(name) <>
        Doc.group(Doc.text("(") <> fieldsDoc <> Doc.text(")")) <>
        bodyDoc
    )
  }
}
case class RecordConstructorCallTerm(
    recordName: Name,
    args: Vector[Term],
    meta: OptionTermMeta
) extends Uneval {
  override type ThisTree = RecordConstructorCallTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): RecordConstructorCallTerm = thisOr(
    copy(args = args.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,`)(args.map(_.toDoc))
    Doc.text(recordName) <> argsDoc
  }
}
case class TraitStmtTerm(
    name: Name,
    uniqId: UniqidOf[TraitStmtTerm] = Uniqid.generate[TraitStmtTerm],
    extendsClause: Option[Term] = None,
    body: Option[BlockTerm] = None,
    meta: OptionTermMeta
) extends TypeDefinition derives ReadWriter {
  override type ThisTree = TraitStmtTerm
  override def switchUniqId(r: UReplacer): TraitStmtTerm = copy(uniqId = r(uniqId))

  override def descent(f: Term => Term, g: TreeMap[Term]): TraitStmtTerm = thisOr(
    copy(
      extendsClause = extendsClause.map(f),
      body = body.map(g)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("trait ") <> Doc.text(name) <> extendsDoc <> bodyDoc
    )
  }
}

case class InterfaceStmtTerm(
    name: Name,
    uniqId: UniqidOf[InterfaceStmtTerm] = Uniqid.generate[InterfaceStmtTerm],
    extendsClause: Option[Term] = None,
    body: Option[BlockTerm] = None,
    meta: OptionTermMeta
) extends TypeDefinition derives ReadWriter {
  override type ThisTree = InterfaceStmtTerm
  override def switchUniqId(r: UReplacer): InterfaceStmtTerm = copy(uniqId = r(uniqId))

  override def descent(f: Term => Term, g: TreeMap[Term]): InterfaceStmtTerm = thisOr(
    copy(
      extendsClause = extendsClause.map(f),
      body = body.map(g)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("interface ") <> Doc.text(name.toString) <> extendsDoc <> bodyDoc
    )
  }
}
case class ObjectStmtTerm(
    name: Name,
    uniqId: UniqidOf[ObjectStmtTerm],
    extendsClause: Option[Term],
    body: Option[BlockTerm],
    meta: OptionTermMeta
) extends TypeDefinition derives ReadWriter {
  override def switchUniqId(r: UReplacer): ObjectStmtTerm = copy(uniqId = r(uniqId))
  override def toDoc(using options: PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    Doc.text("object") <+> Doc.text(name) <+> extendsDoc <+> bodyDoc
  }

  override type ThisTree = ObjectStmtTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectStmtTerm = thisOr(
    copy(
      extendsClause = extendsClause.map(f),
      body = body.map(g)
    )
  )
}

case class ObjectCallTerm(
    objectRef: Term,
    meta: OptionTermMeta
) extends Uneval {
  override def toDoc(using options: PrettierOptions): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  override type ThisTree = ObjectCallTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectCallTerm = thisOr(
    copy(objectRef = f(objectRef))
  )
}
case class ObjectTypeTerm(
    objectDef: ObjectStmtTerm,
    meta: OptionTermMeta
) extends TypeTerm {
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

  override type ThisTree = ObjectTypeTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectTypeTerm = thisOr(
    copy(objectDef = g(objectDef))
  )
}
