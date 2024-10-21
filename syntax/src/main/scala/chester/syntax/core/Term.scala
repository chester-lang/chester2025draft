// TODO: More correctly implement toDoc
package chester.syntax.core

import cats.data.*
import chester.doc.*
import chester.doc.const.{ColorProfile, Docs}
import chester.error.*
import chester.error.ProblemUpickle.*
import chester.syntax.{AbsoluteRef, Name}
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
) extends Term derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

  override type ThisTree = CallingArgTerm
  override def descent(f: Term => Term, g: SpecialMap): CallingArgTerm =
    copy(value = f(value), ty = f(ty))
}

case class Calling(
    args: Vector[CallingArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta
) extends Term derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  override type ThisTree = Calling
  def descent(f: Term => Term, g: SpecialMap): Calling = copy(args = args.map(g))
}

case class FCallTerm(
    f: Term,
    args: Vector[Calling],
    meta: OptionTermMeta
) extends Term {
  override type ThisTree = FCallTerm

  override def toDoc(using options: PrettierOptions): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

  override def descent(op: Term => Term, g: SpecialMap): FCallTerm = thisOr(
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

/** means not changing the subtype of Term */
trait SpecialMap {

  /** note that special rules might apply when x is MetaTerm, which is hard to represent in scala type system */
  def use[T <: Term](x: T): x.ThisTree
  final inline def apply[T <: Term](x: T): T = use(x).asInstanceOf[T]
}

implicit inline def convertSpecialMap[T <: Term](inline f: SpecialMap): T => T = x => f.use(x).asInstanceOf[T]

/** note that this disallow non normal form terms, so avoid using it when non normal form terms are allowed */
type OrM[T <: Term] = (T | MetaTerm)

type EffectsM = OrM[Effects]

given OrMRW[T <: Term](using rw: ReadWriter[Term]): ReadWriter[OrM[T]] =
  rw.asInstanceOf[ReadWriter[OrM[T]]]

/** more abstract Term. sealed trait *T corresponds to sealed trait in Term; trait *C corresponds to case class in Term */
sealed trait TermT[+Rec <: TermT[Rec]] {
  type ThisTree <: TermT[Rec]
  def meta: OptionTermMeta
  def whnf: Trilean
  def toTerm: Term = {
    assert(this.isInstanceOf[Term], "forgot to implement toTerm?")
    this.asInstanceOf[Term]
  }
}

sealed trait Term extends ToDoc with TermT[Term] with ContainsUniqId derives ReadWriter {
  type ThisTree <: Term
  def meta: OptionTermMeta

  def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)

  override def toDoc(using options: PrettierOptions): Doc = toString

  def whnf: Trilean = ???

  protected final inline def thisOr[T <: Term](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  def descent(f: Term => Term, g: SpecialMap): Term
  final def descent(f: Term => Term): Term = descent(
    f,
    new SpecialMap {
      def use[T <: Term](x: T): x.ThisTree = x.descent(f).asInstanceOf[x.ThisTree]
    }
  )
  final def descent2(f: SpecialMap): ThisTree = descent(x => f.use(x), f).asInstanceOf[ThisTree]

  final def descentRecursive(f: Term => Term): Term = thisOr {
    f(descent(_.descentRecursive(f)))
  }

  def inspect(f: Term => Unit): Unit = {
    descent2(new SpecialMap {
      def use[T <: Term](x: T): x.ThisTree = { f(x); x.asInstanceOf[x.ThisTree] }
    })
    ()
  }

  def inspectRecursive(f: Term => Unit): Unit = {
    inspect(_.inspectRecursive(f))
    f(this)
  }

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
  final def substitute[A <: TermWithUniqId](mapping: Seq[(A, Term)]): Term = {
    mapping.foldLeft(this) { case (acc, (from, to)) =>
      acc.substitute(from, to)
    }
  }

  final def substitute(from: TermWithUniqId, to: Term): Term = {
    if (from == to) return this
    if (
      to match {
        case to: TermWithUniqId => from.uniqId == to.uniqId
        case _                  => false
      }
    ) return this
    descentRecursive {
      case x: TermWithUniqId if x.uniqId == from.uniqId => to
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
        descent2(new SpecialMap {
          def use[T <: Term](x: T): x.ThisTree = x.replaceMeta(f).asInstanceOf[x.ThisTree]
        })
    }
  }

  final override def collectU(collector: UCollector): Unit = inspectRecursive {
    case x: TermWithUniqId => collector(x.uniqId)
    case _                 =>
  }

  final override def replaceU(reranger: UReplacer): Term = descentRecursive {
    case x: TermWithUniqId => x.switchUniqId(reranger)
    case x                 => x
  }
}

sealed trait WHNFT[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WHNFT[Rec]
  override def whnf: Trilean = True
}

sealed trait WHNF extends Term with WHNFT[Term] derives ReadWriter {
  override type ThisTree <: WHNF
}

sealed trait UnevalT[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: UnevalT[Rec]
  override def whnf: Trilean = False
}

sealed trait Uneval extends Term with UnevalT[Term] derives ReadWriter {
  override type ThisTree <: Uneval
}
/*
sealed trait SpecialTermT[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: SpecialTermT[Rec]
  override def whnf: Trilean = Unknown
}

sealed trait SpecialTerm extends Term with SpecialTermT[Term] derives ReadWriter {
  override type ThisTree <: SpecialTerm
}
 */
sealed trait TermWithUniqIdT[+Rec <: TermT[Rec]] extends TermT[Rec] with HasUniqId {
  override type ThisTree <: TermWithUniqIdT[Rec]
  override def uniqId: UniqIdOf[Rec]
}

sealed trait TermWithUniqId extends Term with TermWithUniqIdT[Term] derives ReadWriter {
  override type ThisTree <: TermWithUniqId
  override def uniqId: UniqIdOf[Term]
  def switchUniqId(r: UReplacer): TermWithUniqId
}

trait MetaTermC[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: MetaTermC[Rec]
  def impl: HoldNotReadable[?]
  override def toTerm: MetaTerm = MetaTerm(impl, meta)
}

case class MetaTerm(impl: HoldNotReadable[?], meta: OptionTermMeta) extends Term with MetaTermC[Term] {
  override type ThisTree = MetaTerm
  def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.group("Meta" <> Doc.text(impl.toString))

  override def descent(f: Term => Term, g: SpecialMap): MetaTerm = this
}

object MetaTerm {
  @deprecated("meta")
  def from[T](x: T): MetaTerm = MetaTerm(HoldNotReadable(x), meta = None)
}

trait ListTermC[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: ListTermC[Rec]
  def terms: Vector[Rec]
  override def toTerm: ListTerm = ListTerm(terms.map(_.toTerm), meta)
}

case class ListTerm(terms: Vector[Term], meta: OptionTermMeta) extends Term with ListTermC[Term] derives ReadWriter {
  override type ThisTree = ListTerm
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(terms)

  override def descent(f: Term => Term, g: SpecialMap): Term = thisOr(ListTerm(terms.map(f)))
}

object ListTerm {
  @deprecated("meta")
  def apply(terms: Vector[Term]): ListTerm = new ListTerm(terms, meta = None)

  @deprecated("meta")
  def apply(terms: Seq[Term]): ListTerm = new ListTerm(terms.toVector, meta = None)
}

sealed trait TypeTermT[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: TypeTermT[Rec]
}

sealed trait TypeTerm extends Term with TypeTermT[Term] derives ReadWriter {
  override type ThisTree <: TypeTerm
}

sealed trait SortT[+Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: SortT[Rec]
  def level: TermT[Rec]
}

sealed trait Sort extends TypeTerm with SortT[Term] derives ReadWriter {
  override type ThisTree <: Sort
  def level: Term
}

sealed trait TypeT[+Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: TypeT[Rec]
  def level: Rec
}

case class Type(level: Term, meta: OptionTermMeta) extends Sort with TypeT[Term] {
  override type ThisTree = Type
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(Vector(level))

  override def descent(f: Term => Term, g: SpecialMap): Term = thisOr(copy(level = f(level)))
}

trait LevelTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: LevelTypeC[Rec]
}

case class LevelType(meta: OptionTermMeta) extends TypeTerm with WithType with LevelTypeC[Term] {
  override type ThisTree = LevelType
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("LevelType")

  override def descent(f: Term => Term, g: SpecialMap): Term = this

  override def ty: Term = Type0
}

sealed trait LevelT[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: LevelT[Rec]
}

sealed trait Level extends Term with LevelT[Term] with WHNF derives ReadWriter {
  type ThisTree <: Level
}

trait LevelFiniteC[+Rec <: TermT[Rec]] extends LevelT[Rec] {
  override type ThisTree <: LevelFiniteC[Rec]
  def n: Rec
  override def toTerm: LevelFinite = LevelFinite(n.toTerm, meta)
}

case class LevelFinite(n: Term, meta: OptionTermMeta) extends Level with LevelFiniteC[Term] {
  override type ThisTree = LevelFinite
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Level(") <> n.toDoc <> Doc.text(")")

  override def descent(f: Term => Term, g: SpecialMap): LevelFinite =
    thisOr(copy(n = f(n)))
}

trait LevelUnrestrictedC[+Rec <: TermT[Rec]] extends LevelT[Rec] {
  override type ThisTree <: LevelUnrestrictedC[Rec]
  override def toTerm: LevelUnrestricted = LevelUnrestricted(meta)
}

case class LevelUnrestricted(meta: OptionTermMeta) extends Level with LevelUnrestrictedC[Term] {
  override type ThisTree = LevelUnrestricted
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Levelω")

  override def descent(f: Term => Term, g: SpecialMap): LevelUnrestricted = this
}

// Define Level0 using LevelFinite
val Level0 = LevelFinite(IntegerTerm(0, meta = None), meta = None)

val Type0 = Type(Level0, meta = None)

// Referencing Setω in Agda
val Typeω = Type(LevelUnrestricted(None), meta = None)

enum Usage derives ReadWriter {
  case None, Linear, Unrestricted
}

trait PropC[+Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: PropC[Rec]
  def level: Rec
  override def toTerm: Prop = Prop(level.toTerm, meta)
}

case class Prop(level: Term, meta: OptionTermMeta) extends Sort with PropC[Term] {
  override type ThisTree = Prop
  override def descent(f: Term => Term, g: SpecialMap): Term = thisOr(copy(level = f(level)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Prop" <> Docs.`(`, Docs.`)`)(Vector(level))
}

trait FTypeC[+Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: FTypeC[Rec]
  def level: Rec
  override def toTerm: FType = FType(level.toTerm, meta)
}

// fibrant types
case class FType(level: Term, meta: OptionTermMeta) extends Sort with FTypeC[Term] {
  override type ThisTree = FType
  override def descent(f: Term => Term, g: SpecialMap): Term = thisOr(copy(level = f(level)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
}

sealed trait LiteralTermT[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: LiteralTermT[Rec]
}

sealed trait LiteralTerm extends Term with LiteralTermT[Term] derives ReadWriter {
  override type ThisTree <: LiteralTerm
}

sealed trait AbstractIntTermT[+Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: AbstractIntTermT[Rec]
}

sealed trait AbstractIntTerm extends LiteralTerm with AbstractIntTermT[Term] derives ReadWriter {
  override type ThisTree <: AbstractIntTerm
}

trait IntTermC[+Rec <: TermT[Rec]] extends LiteralTermT[Rec] with AbstractIntTermT[Rec] {
  override type ThisTree <: IntTermC[Rec]
  def value: Int
  override def toTerm: IntTerm = IntTerm(value, meta)
}

case class IntTerm(value: Int, meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntTermC[Term] derives ReadWriter {
  override type ThisTree = IntTerm
  override def descent(f: Term => Term, g: SpecialMap): Term = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait IntegerTermC[+Rec <: TermT[Rec]] extends LiteralTermT[Rec] with AbstractIntTermT[Rec] {
  override type ThisTree <: IntegerTermC[Rec]
  def value: BigInt
  override def toTerm: IntegerTerm = IntegerTerm(value, meta)
}

case class IntegerTerm(value: BigInt, meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntegerTermC[Term] derives ReadWriter {
  override type ThisTree = IntegerTerm
  override def descent(f: Term => Term, g: SpecialMap): Term = this

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

sealed trait WithTypeT[+Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WithTypeT[Rec]
  def ty: Rec
}

sealed trait WithType extends Term with WithTypeT[Term] derives ReadWriter {
  override type ThisTree <: WithType
  def ty: Term
}

trait IntegerTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: IntegerTypeC[Rec]
  override def toTerm: IntegerType = IntegerType(meta)
}

case class IntegerType(meta: OptionTermMeta) extends TypeTerm with WithType with IntegerTypeC[Term] derives ReadWriter {
  override type ThisTree = IntegerType
  override def descent(f: Term => Term, g: SpecialMap): IntegerType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Integer", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait IntTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: IntTypeC[Rec]
  override def toTerm: IntType = IntType(meta)
}

// int of 64 bits or more
case class IntType(meta: OptionTermMeta) extends TypeTerm with WithType with IntTypeC[Term] derives ReadWriter {
  override type ThisTree = IntType
  override def descent(f: Term => Term, g: SpecialMap): IntType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Int", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait UIntTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: UIntTypeC[Rec]
  override def toTerm: UIntType = UIntType(meta)
}

// unsigned int of 64 bits or more
case class UIntType(meta: OptionTermMeta) extends TypeTerm with WithType with UIntTypeC[Term] derives ReadWriter {
  override type ThisTree = UIntType
  override def descent(f: Term => Term, g: SpecialMap): UIntType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("UInt", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait NaturalTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: NaturalTypeC[Rec]
  override def toTerm: NaturalType = NaturalType(meta)
}

case class NaturalType(meta: OptionTermMeta) extends TypeTerm with WithType with NaturalTypeC[Term] derives ReadWriter {
  override type ThisTree = NaturalType
  override def descent(f: Term => Term, g: SpecialMap): NaturalType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait RationalTermC[+Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: RationalTermC[Rec]
  def value: Rational
  override def toTerm: RationalTerm = RationalTerm(value, meta)
}

case class RationalTerm(value: Rational, meta: OptionTermMeta) extends LiteralTerm with RationalTermC[Term] derives ReadWriter {
  override type ThisTree = RationalTerm
  override def descent(f: Term => Term, g: SpecialMap): RationalTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait StringTermC[+Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: StringTermC[Rec]
  def value: String
  override def toTerm: StringTerm = StringTerm(value, meta)
}

case class StringTerm(value: String, meta: OptionTermMeta) extends LiteralTerm with StringTermC[Term] derives ReadWriter {
  override type ThisTree = StringTerm
  override def descent(f: Term => Term, g: SpecialMap): StringTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
}

trait SymbolTermC[+Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: SymbolTermC[Rec]
  def value: String
  override def toTerm: SymbolTerm = SymbolTerm(value, meta)
}

case class SymbolTerm(value: String, meta: OptionTermMeta) extends LiteralTerm with SymbolTermC[Term] derives ReadWriter {
  override type ThisTree = SymbolTerm
  override def descent(f: Term => Term, g: SpecialMap): SymbolTerm = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("'" + value, ColorProfile.literalColor)
}

trait RationalTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: RationalTypeC[Rec]
  override def toTerm: RationalType = RationalType(meta)
}

case class RationalType(meta: OptionTermMeta) extends TypeTerm with WithType with RationalTypeC[Term] derives ReadWriter {
  override type ThisTree = RationalType
  override def descent(f: Term => Term, g: SpecialMap): RationalType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Rational", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait FloatTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: FloatTypeC[Rec]
  override def toTerm: FloatType = FloatType(meta)
}

// float of 32 bits or more
case class FloatType(meta: OptionTermMeta) extends TypeTerm with WithType with FloatTypeC[Term] derives ReadWriter {
  override type ThisTree = FloatType
  override def descent(f: Term => Term, g: SpecialMap): FloatType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Float", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait StringTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: StringTypeC[Rec]
  override def toTerm: StringType = StringType(meta)
}

case class StringType(meta: OptionTermMeta) extends TypeTerm with WithType with StringTypeC[Term] derives ReadWriter {
  override type ThisTree = StringType
  override def descent(f: Term => Term, g: SpecialMap): StringType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("String", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait SymbolTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: SymbolTypeC[Rec]
  override def toTerm: SymbolType = SymbolType(meta)
}

case class SymbolType(meta: OptionTermMeta) extends TypeTerm with WithType with SymbolTypeC[Term] derives ReadWriter {
  override type ThisTree = SymbolType
  override def descent(f: Term => Term, g: SpecialMap): SymbolType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Symbol", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait AnyTypeC[+Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: AnyTypeC[Rec]
  def level: Rec
  override def toTerm: AnyType = AnyType(level.toTerm, meta)
}

case class AnyType(level: Term, meta: OptionTermMeta) extends TypeTerm with WithType with AnyTypeC[Term] derives ReadWriter {
  override type ThisTree = AnyType
  override def descent(f: Term => Term, g: SpecialMap): AnyType = thisOr(
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
  override def descent(f: Term => Term, g: SpecialMap): NothingType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Nothing", ColorProfile.typeColor)

  override def ty: Term = Type0
}

implicit val rwUnionHere: ReadWriter[IntegerTerm | SymbolTerm | StringTerm | RationalTerm] =
  union4RW[IntegerTerm, SymbolTerm, StringTerm, RationalTerm]

case class LiteralType(
    literal: IntegerTerm | SymbolTerm | StringTerm | RationalTerm,
    meta: OptionTermMeta
) extends TypeTerm
    with WithType {
  override type ThisTree = LiteralType
  override def descent(f: Term => Term, g: SpecialMap): LiteralType =
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
) extends Term {
  override type ThisTree = ArgTerm
  override def descent(f: Term => Term, g: SpecialMap): ArgTerm = thisOr(
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
) extends Term {
  override type ThisTree = TelescopeTerm
  override def descent(f: Term => Term, g: SpecialMap): TelescopeTerm = thisOr(
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

case class Function(ty: FunctionType, body: Term, meta: OptionTermMeta) extends Term {
  override type ThisTree = Function
  override def descent(f: Term => Term, g: SpecialMap): Function = thisOr(
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
) extends Term {
  override type ThisTree = Matching
  override def descent(f: Term => Term, g: SpecialMap): Matching = thisOr(
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
) extends Term {
  override type ThisTree = FunctionType
  override def descent(f: Term => Term, g: SpecialMap): FunctionType = thisOr(
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
  def apply(
      telescope: Vector[TelescopeTerm],
      resultTy: Term,
      effects: EffectsM = NoEffect,
      meta: OptionTermMeta
  ): FunctionType = {
    new FunctionType(telescope, resultTy, effects, meta)
  }

  @deprecated("meta")
  def apply(telescope: TelescopeTerm, resultTy: Term): FunctionType = {
    new FunctionType(Vector(telescope), resultTy, meta = None)
  }
}

def TyToty: FunctionType = {
  val ty = LocalV("x", Type0, UniqId.generate[LocalV], None)
  FunctionType(TelescopeTerm.from(ArgTerm.from(ty)), ty)
}

case class ObjectClauseValueTerm(
    key: Term,
    value: Term,
    meta: OptionTermMeta
) extends Term derives ReadWriter {
  override type ThisTree = ObjectClauseValueTerm
  override def descent(f: Term => Term, g: SpecialMap): ObjectClauseValueTerm = (
    copy(key = f(key), value = f(value))
  )

  override def toDoc(using options: PrettierOptions): Doc = group(
    key <+> Doc.text("=") <+> value
  )
}

case class ObjectTerm(
    clauses: Vector[ObjectClauseValueTerm],
    meta: OptionTermMeta
) extends Term {
  override type ThisTree = ObjectTerm
  override def descent(f: Term => Term, g: SpecialMap): ObjectTerm = thisOr(
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
) extends Term {
  override type ThisTree = ObjectType
  override def descent(f: Term => Term, g: SpecialMap): ObjectType = thisOr(
    copy(fieldTypes = fieldTypes.map(g))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(
      fieldTypes.map(_.toDoc)
    )
}

sealed trait Builtin extends Term derives ReadWriter {
  override type ThisTree <: Builtin
}

case class ListF(meta: OptionTermMeta) extends Builtin {
  override type ThisTree = ListF
  override def descent(f: Term => Term, g: SpecialMap): ListF = this

  override def toDoc(using options: PrettierOptions): Doc = "List"
}

sealed trait Constructed extends Term derives ReadWriter {
  type ThisTree <: Constructed
}

case class ListType(ty: Term, meta: OptionTermMeta) extends Constructed with TypeTerm {
  override type ThisTree = ListType
  override def descent(f: Term => Term, g: SpecialMap): ListType = thisOr(copy(ty = f(ty)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("List") <> Docs.`(` <> ty <> Docs.`)`
}

case class Union(xs: NonEmptyVector[Term], meta: OptionTermMeta) extends TypeTerm {
  override type ThisTree = Union
  override def descent(f: Term => Term, g: SpecialMap): Union = thisOr(copy(xs = xs.map(g)))

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
  override def descent(f: Term => Term, g: SpecialMap): Intersection = thisOr(
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
sealed trait Effect extends Term derives ReadWriter {
  override type ThisTree <: Effect
  override def descent(f: Term => Term, g: SpecialMap): Effect = this
  def name: String

  override def toDoc(using options: PrettierOptions): Doc = Doc.text(name)
}

extension (e: EffectsM) {
  def nonEmpty: Boolean = e match {
    case e: Effects => e.nonEmpty
    case _          => true
  }
}

case class Effects(effects: Map[LocalV, Term], meta: OptionTermMeta) extends Term derives ReadWriter {
  override type ThisTree = Effects
  override def descent(f: Term => Term, g: SpecialMap): Effects = thisOr(
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
  @deprecated("meta")
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

sealed trait ReferenceCall extends Term with Uneval with TermWithUniqId derives ReadWriter {
  @deprecated("avoid using this")
  def name: Name
  def ty: Term
  override type ThisTree <: ReferenceCall
}

case class LocalV(
    name: Name,
    ty: Term,
    uniqId: UniqIdOf[LocalV],
    meta: OptionTermMeta
) extends ReferenceCall derives ReadWriter {
  override type ThisTree = LocalV
  override def descent(f: Term => Term, g: SpecialMap): LocalV = thisOr(
    copy(ty = f(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(name.toString)

  override def switchUniqId(r: UReplacer): TermWithUniqId = copy(uniqId = r(uniqId))
}

case class ToplevelV(
    id: AbsoluteRef,
    ty: Term,
    uniqId: UniqIdOf[ToplevelV],
    meta: OptionTermMeta
) extends ReferenceCall {
  override type ThisTree = ToplevelV
  override def descent(f: Term => Term, g: SpecialMap): ToplevelV = thisOr(
    copy(ty = f(ty))
  )

  override def name: Name = id.name

  override def toDoc(using options: PrettierOptions): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )

  override def switchUniqId(r: UReplacer): TermWithUniqId = copy(uniqId = r(uniqId))
}

case class ErrorTerm(problem: Problem, meta: OptionTermMeta) extends Term {
  override type ThisTree = ErrorTerm
  override def descent(f: Term => Term, g: SpecialMap): ErrorTerm = this

  override def toDoc(using options: PrettierOptions): Doc = problem.toDoc
}

def ErrorType(error: Problem, meta: OptionTermMeta): ErrorTerm =
  ErrorTerm(error, meta)

sealed trait StmtTerm extends Term derives ReadWriter {
  override type ThisTree <: StmtTerm
}

case class LetStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta
) extends StmtTerm {
  override type ThisTree = LetStmtTerm
  override def descent(f: Term => Term, g: SpecialMap): LetStmtTerm = thisOr(
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
  override def descent(f: Term => Term, g: SpecialMap): DefStmtTerm = thisOr(
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
  override def descent(f: Term => Term, g: SpecialMap): ExprStmtTerm = thisOr(
    copy(expr = f(expr), ty = f(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc = expr.toDoc
}

case class NonlocalOrLocalReturn(value: Term, meta: OptionTermMeta) extends StmtTerm {
  override type ThisTree = NonlocalOrLocalReturn
  override def descent(f: Term => Term, g: SpecialMap): NonlocalOrLocalReturn = thisOr(
    copy(value = f(value))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("return") <+> value.toDoc
}

case class TupleType(types: Vector[Term], meta: OptionTermMeta) extends TypeTerm {
  override type ThisTree = TupleType
  override def descent(f: Term => Term, g: SpecialMap): TupleType = thisOr(
    copy(types = types.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`, ",")(types)
  }
}

case class TupleTerm(values: Vector[Term], meta: OptionTermMeta) extends Term {
  override type ThisTree = TupleTerm
  override def descent(f: Term => Term, g: SpecialMap): TupleTerm = thisOr(
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
) extends Term derives ReadWriter {
  override type ThisTree = BlockTerm
  override def descent(f: Term => Term, g: SpecialMap): BlockTerm = thisOr(
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

object BlockTerm {
  @deprecated("meta")
  def apply(stmts: Vector[StmtTerm], value: Term): BlockTerm =
    new BlockTerm(stmts, value, meta = None)

  @deprecated("meta")
  def apply(stmts: Seq[StmtTerm], value: Term): BlockTerm =
    new BlockTerm(stmts.toVector, value, meta = None)
}

case class Annotation(
    term: Term,
    ty: Option[Term],
    effects: Option[EffectsM],
    meta: OptionTermMeta
) extends Term {
  override type ThisTree = Annotation
  override def descent(f: Term => Term, g: SpecialMap): Annotation = thisOr(
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

// TODO: tuple?
def UnitType(meta: OptionTermMeta) =
  ObjectType(Vector.empty, meta = meta)
def UnitTerm(meta: OptionTermMeta) =
  ObjectTerm(Vector.empty, meta = meta)

case class FieldTerm(
    name: Name,
    ty: Term,
    meta: OptionTermMeta
) extends Term derives ReadWriter {
  override type ThisTree = FieldTerm
  override def descent(f: Term => Term, g: SpecialMap): FieldTerm = thisOr(
    copy(ty = f(ty))
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(name) <> Doc.text(": ") <> ty.toDoc
}

sealed trait TypeDefinition extends StmtTerm with TermWithUniqId derives ReadWriter {
  def name: Name
  def uniqId: UniqIdOf[TypeDefinition]

  override def switchUniqId(r: UReplacer): TypeDefinition
}

case class RecordStmtTerm(
    name: Name,
    uniqId: UniqIdOf[RecordStmtTerm] = UniqId.generate[RecordStmtTerm],
    fields: Vector[FieldTerm],
    body: Option[BlockTerm],
    meta: OptionTermMeta
) extends TypeDefinition {
  override type ThisTree = RecordStmtTerm
  override def switchUniqId(r: UReplacer): RecordStmtTerm = copy(uniqId = r(uniqId))
  override def descent(f: Term => Term, g: SpecialMap): RecordStmtTerm = thisOr(
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
) extends Term {
  override type ThisTree = RecordConstructorCallTerm
  override def descent(f: Term => Term, g: SpecialMap): RecordConstructorCallTerm = thisOr(
    copy(args = args.map(f))
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,`)(args.map(_.toDoc))
    Doc.text(recordName) <> argsDoc
  }
}
case class TraitStmtTerm(
    name: Name,
    uniqId: UniqIdOf[TraitStmtTerm] = UniqId.generate[TraitStmtTerm],
    extendsClause: Option[Term] = None,
    body: Option[BlockTerm] = None,
    meta: OptionTermMeta
) extends TypeDefinition derives ReadWriter {
  override type ThisTree = TraitStmtTerm
  override def switchUniqId(r: UReplacer): TraitStmtTerm = copy(uniqId = r(uniqId))

  override def descent(f: Term => Term, g: SpecialMap): TraitStmtTerm = thisOr(
    copy(
      extendsClause = extendsClause.map(f),
      body = body.map(g)
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("trait ") <> Doc.text(name.toString) <> extendsDoc <> bodyDoc
    )
  }
}

case class InterfaceStmtTerm(
    name: Name,
    uniqId: UniqIdOf[InterfaceStmtTerm] = UniqId.generate[InterfaceStmtTerm],
    extendsClause: Option[Term] = None,
    body: Option[BlockTerm] = None,
    meta: OptionTermMeta
) extends TypeDefinition derives ReadWriter {
  override type ThisTree = InterfaceStmtTerm
  override def switchUniqId(r: UReplacer): InterfaceStmtTerm = copy(uniqId = r(uniqId))

  override def descent(f: Term => Term, g: SpecialMap): InterfaceStmtTerm = thisOr(
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
    uniqId: UniqIdOf[ObjectStmtTerm],
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
  override def descent(f: Term => Term, g: SpecialMap): ObjectStmtTerm = thisOr(
    copy(
      extendsClause = extendsClause.map(f),
      body = body.map(g)
    )
  )
}

case class ObjectCallTerm(
    objectRef: Term,
    meta: OptionTermMeta
) extends Term {
  override def toDoc(using options: PrettierOptions): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  override type ThisTree = ObjectCallTerm
  override def descent(f: Term => Term, g: SpecialMap): ObjectCallTerm = thisOr(
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
  override def descent(f: Term => Term, g: SpecialMap): ObjectTypeTerm = thisOr(
    copy(objectDef = g(objectDef))
  )
}
case class TodoTerm(meta: OptionTermMeta) extends Term {
  override def toDoc(using options: PrettierOptions): Doc = Doc.text("TODO")
  override type ThisTree = TodoTerm
  override def descent(f: Term => Term, g: SpecialMap): TodoTerm = this
}
