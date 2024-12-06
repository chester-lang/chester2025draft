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

/* Type Hierarchy Naming Conventions:
 *
 * The codebase follows these suffix patterns:
 *
 * - *C (e.g. LocalVC): Abstract "case class-like" traits that define the structure and behavior
 *   for a specific term type. These contain the common fields and methods that will be
 *   implemented by concrete case classes.
 *
 * - *F (e.g. LocalVF): Function interfaces used for constructing terms. These define
 *   factory methods that create instances of the corresponding *C types.
 *   They enable flexible term construction while maintaining type safety.
 *
 * - *T (e.g. TermT): Abstract "trait-like" interfaces that define the core behavior
 *   for a category of terms. These represent the fundamental abstractions and typically
 *   extend other trait hierarchies.
 *
 * Example hierarchy:
 * - TermT[Rec] - Base trait for all terms
 *   - LocalVC[Rec] - Structure for local variables
 *     - LocalV - Concrete implementation
 *   - LocalVF[Rec, ThisTree] - Factory for creating LocalV instances
 */

case class TermMeta(sourcePos: SourcePos) derives ReadWriter

type OptionTermMeta = Option[TermMeta]

@FunctionalInterface
trait CallingArgTermF[Rec <: TermT[Rec], ThisTree <: CallingArgTermC[Rec]] {
  def newCallingArgTerm(value: Rec, ty: Rec, name: Option[Name], vararg: Boolean, meta: OptionTermMeta): ThisTree
}

trait CallingArgTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: CallingArgTermC[Rec]
  def value: Rec
  def ty: Rec
  def name: Option[Name]
  def vararg: Boolean
  override def toTerm: CallingArgTerm = CallingArgTerm(value.toTerm, ty.toTerm, name, vararg, meta)
  def cons: CallingArgTermF[Rec, ThisTree]

  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

  def cpy(
      value: Rec = value,
      ty: Rec = ty,
      name: Option[Name] = name,
      vararg: Boolean = vararg,
      meta: OptionTermMeta = meta
  ): ThisTree =
    cons.newCallingArgTerm(value, ty, name, vararg, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(value = f(value), ty = f(ty))
  )
}

case class CallingArgTerm(
    value: Term,
    ty: Term,
    name: Option[Name] = None,
    vararg: Boolean = false,
    meta: OptionTermMeta
) extends WHNF
    with CallingArgTermC[Term] derives ReadWriter {
  override def cons: CallingArgTermF[Term, ThisTree] = this.copy
  override type ThisTree = CallingArgTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): CallingArgTerm =
    copy(value = f(value), ty = f(ty))
}

@FunctionalInterface
trait CallingF[Rec <: TermT[Rec], ThisTree <: CallingC[Rec]] {
  def newCalling(args: Vector[CallingArgTermC[Rec]], implicitly: Boolean, meta: OptionTermMeta): ThisTree
}

trait CallingC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: CallingC[Rec]
  def args: Vector[CallingArgTermC[Rec]]
  def implicitly: Boolean
  override def toTerm: Calling = Calling(args.map(_.toTerm), implicitly, meta)
  def cons: CallingF[Rec, ThisTree]

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  def cpy(
      args: Vector[CallingArgTermC[Rec]] = args,
      implicitly: Boolean = implicitly,
      meta: OptionTermMeta = meta
  ): ThisTree =
    cons.newCalling(args, implicitly, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(args = args.map(g))
  )
}

case class Calling(
    args: Vector[CallingArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta
) extends WHNF
    with CallingC[Term] derives ReadWriter {
  override type ThisTree = Calling
  override def cons: CallingF[Term, ThisTree] = this.copy
  override def descent(f: Term => Term, g: TreeMap[Term]): Calling =
    copy(args = args.map(g))
}

@FunctionalInterface
trait FCallTermF[Rec <: TermT[Rec], ThisTree <: FCallTermC[Rec]] {
  def newFCallTerm(f: Rec, args: Vector[CallingC[Rec]], meta: OptionTermMeta): ThisTree
}

trait FCallTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: FCallTermC[Rec]
  def f: Rec
  def args: Vector[CallingC[Rec]]
  override def toTerm: FCallTerm = FCallTerm(f.toTerm, args.map(_.toTerm), meta)
  def cons: FCallTermF[Rec, ThisTree]

  override def toDoc(using options: PrettierOptions): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

  def cpy(f: Rec = f, args: Vector[CallingC[Rec]] = args, meta: OptionTermMeta = meta): ThisTree =
    cons.newFCallTerm(f, args, meta)

  def descent(a: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(f = a(f), args = args.map(g))
  )
}

case class FCallTerm(
    f: Term,
    args: Vector[Calling],
    meta: OptionTermMeta
) extends WHNF
    with FCallTermC[Term] {
  override type ThisTree = FCallTerm
  override def cons: FCallTermF[Term, ThisTree] = this.copy
  override def descent(a: Term => Term, g: TreeMap[Term]): FCallTerm = thisOr(
    copy(f = a(f), args = args.map(g))
  )
}

object FCallTerm {}

trait PatT[Rec <: TermT[Rec]] extends SpecialTermT[Rec] {
  override type ThisTree <: PatT[Rec]
}

sealed trait Pat extends SpecialTerm with PatT[Term] derives ReadWriter {
  override type ThisTree <: Pat
}

@FunctionalInterface
trait BindF[Rec <: TermT[Rec], ThisTree <: BindC[Rec]] {
  def newBind(bind: LocalVC[Rec], ty: Rec, meta: OptionTermMeta): ThisTree
}

trait BindC[Rec <: TermT[Rec]] extends PatT[Rec] {
  override type ThisTree <: BindC[Rec]

  def bind: LocalVC[Rec]
  def ty: Rec
  def cons: BindF[Rec, ThisTree]

  override def toTerm: Bind = Bind(bind.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = bind.toDoc <+> Docs.`:` <+> ty.toDoc

  def cpy(bind: LocalVC[Rec] = bind, ty: Rec = ty, meta: OptionTermMeta = meta): ThisTree = cons.newBind(bind, ty, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(cpy(bind = g(bind), ty = f(ty)))
}

case class Bind(
    bind: LocalV,
    ty: Term,
    meta: OptionTermMeta
) extends Pat
    with BindC[Term] {
  override type ThisTree = Bind
  override def cons: BindF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(bind = g(bind), ty = f(ty)))
}

object Bind {
  @deprecated("meta")
  def from(bind: LocalV): Bind = Bind(bind, bind.ty, None)
}

/** more abstract Term. sealed trait *T corresponds to sealed trait in Term; trait *C corresponds to case class in Term */
trait TermT[Rec <: TermT[Rec]] extends Any with ToDoc with Tree[Rec] {
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

trait WHNFT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WHNFT[Rec]
  override def whnf: Trilean = True
}

sealed trait WHNF extends Term with WHNFT[Term] derives ReadWriter {
  override type ThisTree <: WHNF
}

trait UnevalT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: UnevalT[Rec]
  override def whnf: Trilean = False
}

sealed trait Uneval extends Term with UnevalT[Term] derives ReadWriter {
  override type ThisTree <: Uneval
}
trait SpecialTermT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: SpecialTermT[Rec]
  override def whnf: Trilean = Unknown
}

sealed trait SpecialTerm extends Term with SpecialTermT[Term] derives ReadWriter {
  override type ThisTree <: SpecialTerm
}
trait TermWithUniqidT[Rec <: TermT[Rec]] extends TermT[Rec] with HasUniqid {
  override type ThisTree <: TermWithUniqidT[Rec]
  override def uniqId: UniqidOf[TermT[Rec]]
}

sealed trait TermWithUniqid extends Term with TermWithUniqidT[Term] derives ReadWriter {
  override type ThisTree <: TermWithUniqid
  override def uniqId: UniqidOf[Term]
  def switchUniqId(r: UReplacer): TermWithUniqid
}

trait EffectsMT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: EffectsMT[Rec]
}

sealed trait EffectsM extends Term with EffectsMT[Term] derives ReadWriter {
  override type ThisTree <: EffectsM
}

trait MetaTermC[Rec <: TermT[Rec]] extends TermT[Rec] with EffectsMT[Rec] with SpecialTermT[Rec] {
  override type ThisTree <: MetaTermC[Rec]
  def impl: HoldNotReadable[?]
  override def toTerm: MetaTerm = MetaTerm(impl, meta)
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.group("Meta" <> Doc.text(impl.toString))

  def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = this
}

case class MetaTerm(impl: HoldNotReadable[?], meta: OptionTermMeta) extends Term with MetaTermC[Term] with EffectsM with SpecialTerm {
  override type ThisTree = MetaTerm
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

trait TypeTermT[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: TypeTermT[Rec]
}

sealed trait TypeTerm extends Term with TypeTermT[Term] with WHNF derives ReadWriter {
  override type ThisTree <: TypeTerm
}

trait SortT[Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: SortT[Rec]
  def level: TermT[Rec]
}

sealed trait Sort extends TypeTerm with SortT[Term] derives ReadWriter {
  override type ThisTree <: Sort
  def level: Term
}

trait TypeT[Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: TypeT[Rec]
  def level: Rec
  override def toTerm: Type = Type(level.toTerm, meta)
}

case class Type(level: Term, meta: OptionTermMeta) extends Sort with TypeT[Term] {
  override type ThisTree = Type
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(Vector(level))

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))
}

trait LevelTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: LevelTypeC[Rec]
  override def toTerm: LevelType = LevelType(meta)
}

case class LevelType(meta: OptionTermMeta) extends TypeTerm with WithType with LevelTypeC[Term] {
  override type ThisTree = LevelType
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("LevelType")

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

  override def ty: Term = Type0
}

trait LevelT[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WHNFT[Rec] {
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
  def copy(level: Rec = level, meta: OptionTermMeta = meta): ThisTree
  override def toTerm: FType = FType(level.toTerm, meta)
  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(copy(level = f(level)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
}

// fibrant types
case class FType(level: Term, meta: OptionTermMeta) extends Sort with FTypeC[Term] {
  override type ThisTree = FType
}

trait LiteralTermT[Rec <: TermT[Rec]] extends TermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: LiteralTermT[Rec]
}

sealed trait LiteralTerm extends Term with LiteralTermT[Term] with WHNF derives ReadWriter {
  override type ThisTree <: LiteralTerm
}

trait AbstractIntTermT[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
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

trait WithTypeT[Rec <: TermT[Rec]] extends TermT[Rec] {
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

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

case class BooleanTerm(value: Boolean, meta: OptionTermMeta) extends LiteralTerm with BooleanTermC[Term] derives ReadWriter {
  override type ThisTree = BooleanTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): BooleanTerm = this

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

trait NothingTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: NothingTypeC[Rec]
  override def toTerm: NothingType = NothingType(meta)
}

case class NothingType(meta: OptionTermMeta) extends TypeTerm with WithType with NothingTypeC[Term] {
  override type ThisTree = NothingType
  override def descent(f: Term => Term, g: TreeMap[Term]): NothingType = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("Nothing", ColorProfile.typeColor)

  override def ty: Term = Type0
}

trait LiteralTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: LiteralTypeC[Rec]
  def literal: LiteralTermT[Rec]
  override def toTerm: LiteralType = LiteralType(literal.toTerm.asInstanceOf[LiteralTerm], meta)
}

case class LiteralType(
    literal: LiteralTerm,
    meta: OptionTermMeta
) extends TypeTerm
    with WithType
    with LiteralTypeC[Term] {
  override type ThisTree = LiteralType
  override def descent(f: Term => Term, g: TreeMap[Term]): LiteralType =
    copy(literal = g(literal).asInstanceOf[LiteralTerm])

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(literal.toString, ColorProfile.typeColor)

  override def ty: Term = Type0
}

@FunctionalInterface
trait ArgTermF[Rec <: TermT[Rec], ThisTree <: ArgTermC[Rec]] {
  def newArgTerm(bind: LocalV, ty: Rec, default: Option[Rec], vararg: Boolean, meta: OptionTermMeta): ThisTree
}

trait ArgTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: ArgTermC[Rec]

  def bind: LocalV
  def ty: Rec
  def default: Option[Rec]
  def vararg: Boolean
  def cons: ArgTermF[Rec, ThisTree]

  override def toTerm: ArgTerm = ArgTerm(bind, ty.toTerm, default.map(_.toTerm), vararg, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
    bind.toDoc <> varargDoc <> Docs.`:` <+> ty.toDoc <> defaultDoc
  }

  def cpy(bind: LocalV = bind, ty: Rec = ty, default: Option[Rec] = default, vararg: Boolean = vararg, meta: OptionTermMeta = meta): ThisTree =
    cons.newArgTerm(bind, ty, default, vararg, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(bind = g(bind), ty = f(ty), default = default.map(f))
  )

  def name = bind.name
}

case class ArgTerm(
    bind: LocalV,
    ty: Term,
    default: Option[Term] = None,
    vararg: Boolean = false,
    meta: OptionTermMeta
) extends WHNF
    with ArgTermC[Term] {
  override type ThisTree = ArgTerm
  override def cons: ArgTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ArgTerm = thisOr(
    copy(bind = g(bind), ty = f(ty), default = default.map(f))
  )
}

object ArgTerm {

  @deprecated("meta")
  def from(bind: LocalV): ArgTerm = ArgTerm(bind, bind.ty, meta = None)
}

@FunctionalInterface
trait TelescopeTermF[Rec <: TermT[Rec], ThisTree <: TelescopeTermC[Rec]] {
  def newTelescope(
      args: Vector[ArgTermC[Rec]],
      implicitly: Boolean,
      meta: OptionTermMeta
  ): ThisTree
}

trait TelescopeTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: TelescopeTermC[Rec]
  def args: Vector[ArgTermC[Rec]]
  def implicitly: Boolean
  def cons: TelescopeTermF[Rec, ThisTree]

  override def toTerm: TelescopeTerm = TelescopeTerm(args.map(_.toTerm), implicitly, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc =
      args.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    if (implicitly) {
      Docs.`[` <> argsDoc <> Docs.`]`
    } else {
      Docs.`(` <> argsDoc <> Docs.`)`
    }
  }

  def cpy(
      args: Vector[ArgTermC[Rec]] = args,
      implicitly: Boolean = implicitly,
      meta: OptionTermMeta = meta
  ): ThisTree = cons.newTelescope(args, implicitly, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(args = args.map(g))
  )
}

object TelescopeTerm {
  @deprecated("meta")
  def from(x: ArgTerm*): TelescopeTerm = TelescopeTerm(x.toVector, meta = None)
}

case class TelescopeTerm(
    args: Vector[ArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta
) extends WHNF
    with TelescopeTermC[Term] {
  override type ThisTree = TelescopeTerm
  override def cons: TelescopeTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): TelescopeTerm = thisOr(
    copy(args = args.map(g))
  )
}

@FunctionalInterface
trait FunctionF[Rec <: TermT[Rec], ThisTree <: FunctionC[Rec]] {
  def newFunction(ty: FunctionTypeC[Rec], body: Rec, meta: OptionTermMeta): ThisTree
}

trait FunctionC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: FunctionC[Rec]

  def ty: FunctionTypeC[Rec]
  def body: Rec
  def cons: FunctionF[Rec, ThisTree]

  override def toTerm: Function = Function(ty.toTerm, body.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val paramsDoc = ty.telescope.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val returnTypeDoc = Docs.`:` <+> ty.resultTy.toDoc
    val effectsDoc = if (ty.effects.nonEmpty) {
      Docs.`/` <+> ty.effects.toDoc
    } else {
      Doc.empty
    }
    val bodyDoc = body.toDoc
    group(paramsDoc <> returnTypeDoc <+> Docs.`=>` <+> bodyDoc <> effectsDoc)
  }

  def cpy(ty: FunctionTypeC[Rec] = ty, body: Rec = body, meta: OptionTermMeta = meta): ThisTree = cons.newFunction(ty, body, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(cpy(ty = g(ty), body = f(body)))
}

case class Function(
    ty: FunctionType,
    body: Term,
    meta: OptionTermMeta
) extends WHNF
    with FunctionC[Term] {
  override type ThisTree = Function
  override def cons: FunctionF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): Function = thisOr(copy(ty = g(ty), body = f(body)))
}

@deprecated("not used")
case class MatchingClause(meta: OptionTermMeta) extends WHNF {
  override type ThisTree = MatchingClause
  override def descent(f: Term => Term, g: TreeMap[Term]): MatchingClause = this
  override def toDoc(using options: PrettierOptions): Doc = toString // TODO

}

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

@FunctionalInterface
trait FunctionTypeF[Rec <: TermT[Rec], ThisTree <: FunctionTypeC[Rec]] {
  def newFunctionType(
      telescope: Vector[TelescopeTermC[Rec]],
      resultTy: Rec,
      effects: EffectsM,
      meta: OptionTermMeta
  ): ThisTree
}

trait FunctionTypeC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: FunctionTypeC[Rec]

  def telescope: Vector[TelescopeTermC[Rec]]
  def resultTy: Rec
  def effects: EffectsM
  def cons: FunctionTypeF[Rec, ThisTree]

  override def toTerm: FunctionType = FunctionType(
    telescope.map(_.toTerm),
    resultTy.toTerm,
    effects,
    meta
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

  def cpy(
      telescope: Vector[TelescopeTermC[Rec]] = telescope,
      resultTy: Rec = resultTy,
      effects: EffectsM = effects,
      meta: OptionTermMeta = meta
  ): ThisTree = cons.newFunctionType(telescope, resultTy, effects, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(
      telescope = telescope.map(g),
      resultTy = f(resultTy),
      effects = g(effects)
    )
  )
}

case class FunctionType(
    telescope: Vector[TelescopeTerm],
    resultTy: Term,
    effects: EffectsM = NoEffect,
    meta: OptionTermMeta
) extends WHNF
    with FunctionTypeC[Term] {
  override type ThisTree = FunctionType
  override def cons: FunctionTypeF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): FunctionType = thisOr(
    copy(
      telescope = telescope.map(g),
      resultTy = f(resultTy),
      effects = g(effects)
    )
  )
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

@FunctionalInterface
trait ObjectClauseValueTermF[Rec <: TermT[Rec], ThisTree <: ObjectClauseValueTermC[Rec]] {
  def newObjectClauseValueTerm(key: Rec, value: Rec, meta: OptionTermMeta): ThisTree
}

trait ObjectClauseValueTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: ObjectClauseValueTermC[Rec]
  def key: Rec
  def value: Rec
  def cons: ObjectClauseValueTermF[Rec, ThisTree]

  override def toTerm: ObjectClauseValueTerm = ObjectClauseValueTerm(key.toTerm, value.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = group(
    key.toDoc <+> Doc.text("=") <+> value.toDoc
  )

  def cpy(key: Rec = key, value: Rec = value, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectClauseValueTerm(key, value, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(key = f(key), value = f(value))
  )
}

case class ObjectClauseValueTerm(
    key: Term,
    value: Term,
    meta: OptionTermMeta
) extends WHNF
    with ObjectClauseValueTermC[Term] derives ReadWriter {
  override type ThisTree = ObjectClauseValueTerm
  override def cons: ObjectClauseValueTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectClauseValueTerm = (
    copy(key = f(key), value = f(value))
  )
}

@FunctionalInterface
trait ObjectTermF[Rec <: TermT[Rec], ThisTree <: ObjectTermC[Rec]] {
  def newObjectTerm(clauses: Vector[ObjectClauseValueTermC[Rec]], meta: OptionTermMeta): ThisTree
}

trait ObjectTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: ObjectTermC[Rec]
  def clauses: Vector[ObjectClauseValueTermC[Rec]]
  def cons: ObjectTermF[Rec, ThisTree]

  override def toTerm: ObjectTerm = ObjectTerm(clauses.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc))

  def cpy(clauses: Vector[ObjectClauseValueTermC[Rec]] = clauses, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectTerm(clauses, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(clauses = clauses.map(g))
  )
}

case class ObjectTerm(
    clauses: Vector[ObjectClauseValueTerm],
    meta: OptionTermMeta
) extends WHNF
    with ObjectTermC[Term] {
  override type ThisTree = ObjectTerm
  override def cons: ObjectTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectTerm = thisOr(
    copy(clauses = clauses.map(g))
  )
}

// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
@FunctionalInterface
trait ObjectTypeF[Rec <: TermT[Rec], ThisTree <: ObjectTypeC[Rec]] {
  def newObjectType(fieldTypes: Vector[ObjectClauseValueTermC[Rec]], exactFields: Boolean, meta: OptionTermMeta): ThisTree
}

trait ObjectTypeC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: ObjectTypeC[Rec]
  def fieldTypes: Vector[ObjectClauseValueTermC[Rec]]
  def exactFields: Boolean
  def cons: ObjectTypeF[Rec, ThisTree]

  override def toTerm: ObjectType = ObjectType(fieldTypes.map(_.toTerm), exactFields, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(
      fieldTypes.map(_.toDoc)
    )

  def cpy(fieldTypes: Vector[ObjectClauseValueTermC[Rec]] = fieldTypes, exactFields: Boolean = exactFields, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectType(fieldTypes, exactFields, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(fieldTypes = fieldTypes.map(g))
  )
}

case class ObjectType(
    fieldTypes: Vector[ObjectClauseValueTerm],
    exactFields: Boolean = false,
    meta: OptionTermMeta
) extends WHNF
    with ObjectTypeC[Term] {
  override type ThisTree = ObjectType
  override def cons: ObjectTypeF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectType = thisOr(
    copy(fieldTypes = fieldTypes.map(g))
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
    copy(effects = effects.map { case (k, v) => (g(k), f(v)) })
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
    (a.replaceMeta(f).asInstanceOf[LocalV], b.replaceMeta(f))
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

trait ReferenceCallC[Rec <: TermT[Rec]] extends UnevalT[Rec] with TermWithUniqidT[Rec] {
  override type ThisTree <: ReferenceCallC[Rec]
  @deprecated("dont use")
  def name: Name
  def ty: Rec

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec
}

sealed trait ReferenceCall extends Term with Uneval with TermWithUniqid with ReferenceCallC[Term] derives ReadWriter {
  override type ThisTree <: ReferenceCall
}

@FunctionalInterface
trait LocalVF[Rec <: TermT[Rec], ThisTree <: LocalVC[Rec]] {
  def newLocalV(name: Name, ty: Rec, uniqId: UniqidOf[LocalVC[Rec]], meta: OptionTermMeta): ThisTree
}

trait LocalVC[Rec <: TermT[Rec]] extends ReferenceCallC[Rec] {
  override type ThisTree <: LocalVC[Rec]

  @deprecated("dont use")
  def name: Name
  def ty: Rec
  def uniqId: UniqidOf[LocalVC[Rec]]
  def cons: LocalVF[Rec, ThisTree]

  override def toTerm: LocalV = LocalV(name, ty.toTerm, uniqId, meta)

  override def toDoc(using options: PrettierOptions): Doc = Doc.text(name)

  def cpy(name: Name = name, ty: Rec = ty, uniqId: UniqidOf[LocalVC[Rec]] = uniqId, meta: OptionTermMeta = meta): ThisTree =
    cons.newLocalV(name, ty, uniqId, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(cpy(ty = f(ty)))
}

implicit def LocalVConversion[Rec <: TermT[Rec]](x: UniqidOf[LocalVC[Rec]]): UniqidOf[LocalV] = x.asInstanceOf[UniqidOf[LocalV]]

case class LocalV(
    name: Name,
    ty: Term,
    uniqId: UniqidOf[LocalV],
    meta: OptionTermMeta
) extends ReferenceCall
    with LocalVC[Term] {
  override type ThisTree = LocalV
  override def cons: LocalVF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): LocalV = thisOr(copy(ty = f(ty)))

  override def switchUniqId(r: UReplacer): LocalV = copy(uniqId = r(uniqId))
}

@FunctionalInterface
trait ToplevelVF[Rec <: TermT[Rec], ThisTree <: ToplevelVC[Rec]] {
  def newToplevelV(id: AbsoluteRef, ty: Rec, uniqId: UniqidOf[ToplevelVC[Rec]], meta: OptionTermMeta): ThisTree
}

trait ToplevelVC[Rec <: TermT[Rec]] extends ReferenceCallC[Rec] {
  override type ThisTree <: ToplevelVC[Rec]

  def id: AbsoluteRef
  def ty: Rec
  def uniqId: UniqidOf[ToplevelVC[Rec]]
  def cons: ToplevelVF[Rec, ThisTree]

  override def toTerm: ToplevelV = ToplevelV(id, ty.toTerm, uniqId, meta)

  override def toDoc(using options: PrettierOptions): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )

  def cpy(id: AbsoluteRef = id, ty: Rec = ty, uniqId: UniqidOf[ToplevelVC[Rec]] = uniqId, meta: OptionTermMeta = meta): ThisTree =
    cons.newToplevelV(id, ty, uniqId, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(cpy(ty = f(ty)))

  @deprecated("dont use")
  def name = id.name
}

implicit def conversionTop[Rec <: TermT[Rec]](x: UniqidOf[ToplevelVC[Rec]]): UniqidOf[ToplevelV] = x.asInstanceOf[UniqidOf[ToplevelV]]

case class ToplevelV(
    id: AbsoluteRef,
    ty: Term,
    uniqId: UniqidOf[ToplevelV],
    meta: OptionTermMeta
) extends ReferenceCall
    with ToplevelVC[Term] {
  override type ThisTree = ToplevelV
  override def cons: ToplevelVF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ToplevelV = thisOr(copy(ty = f(ty)))

  override def switchUniqId(r: UReplacer): ToplevelV = copy(uniqId = r(uniqId))
}

trait ErrorTermC[Rec <: TermT[Rec]] extends SpecialTermT[Rec] {
  override type ThisTree <: ErrorTermC[Rec]
  def problem: Problem
  override def toTerm: ErrorTerm = ErrorTerm(problem, meta)

  override def toDoc(using options: PrettierOptions): Doc = problem.toDoc

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = this
}

case class ErrorTerm(problem: Problem, meta: OptionTermMeta) extends SpecialTerm with ErrorTermC[Term] {
  override type ThisTree = ErrorTerm
}

def ErrorType(error: Problem, meta: OptionTermMeta): ErrorTerm =
  ErrorTerm(error, meta)

trait StmtTermT[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: StmtTermT[Rec]
}

sealed trait StmtTerm extends Term with StmtTermT[Term] derives ReadWriter {
  override type ThisTree <: StmtTerm
}

@FunctionalInterface
trait LetStmtTermF[Rec <: TermT[Rec], ThisTree <: LetStmtTermC[Rec]] {
  def newLetStmt(localv: LocalVC[Rec], value: Rec, ty: Rec, meta: OptionTermMeta): ThisTree
}

trait LetStmtTermC[Rec <: TermT[Rec]] extends StmtTermT[Rec] {
  override type ThisTree <: LetStmtTermC[Rec]
  def localv: LocalVC[Rec]
  def value: Rec
  def ty: Rec
  def cons: LetStmtTermF[Rec, ThisTree]
  override def toTerm: LetStmtTerm = LetStmtTerm(localv.toTerm, value.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }

  def cpy(localv: LocalVC[Rec] = localv, value: Rec = value, ty: Rec = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.newLetStmt(localv, value, ty, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}

case class LetStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta
) extends StmtTerm
    with LetStmtTermC[Term] {
  override type ThisTree = LetStmtTerm
  override def cons: LetStmtTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): LetStmtTerm = thisOr(
    copy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}

@FunctionalInterface
trait DefStmtTermF[Rec <: TermT[Rec], ThisTree <: DefStmtTermC[Rec]] {
  def newDefStmt(localv: LocalVC[Rec], value: Rec, ty: Rec, meta: OptionTermMeta): ThisTree
}

trait DefStmtTermC[Rec <: TermT[Rec]] extends StmtTermT[Rec] {
  override type ThisTree <: DefStmtTermC[Rec]
  def localv: LocalVC[Rec]
  def value: Rec
  def ty: Rec
  def cons: DefStmtTermF[Rec, ThisTree]
  override def toTerm: DefStmtTerm = DefStmtTerm(localv.toTerm, value.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }

  def cpy(localv: LocalVC[Rec] = localv, value: Rec = value, ty: Rec = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.newDefStmt(localv, value, ty, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}

case class DefStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta
) extends StmtTerm
    with DefStmtTermC[Term] {
  override type ThisTree = DefStmtTerm
  override def cons: DefStmtTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): DefStmtTerm = thisOr(
    copy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}

@FunctionalInterface
trait ExprStmtTermF[Rec <: TermT[Rec], ThisTree <: ExprStmtTermC[Rec]] {
  def newExprStmt(expr: Rec, ty: Rec, meta: OptionTermMeta): ThisTree
}

trait ExprStmtTermC[Rec <: TermT[Rec]] extends StmtTermT[Rec] {
  override type ThisTree <: ExprStmtTermC[Rec]
  def expr: Rec
  def ty: Rec
  def cons: ExprStmtTermF[Rec, ThisTree]

  override def toTerm: ExprStmtTerm = ExprStmtTerm(expr.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = expr.toDoc

  def cpy(expr: Rec = expr, ty: Rec = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.newExprStmt(expr, ty, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(expr = f(expr), ty = f(ty))
  )
}

case class ExprStmtTerm(
    expr: Term,
    ty: Term = AnyType0,
    meta: OptionTermMeta
) extends StmtTerm
    with ExprStmtTermC[Term] {
  override type ThisTree = ExprStmtTerm
  override def cons: ExprStmtTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ExprStmtTerm = thisOr(
    copy(expr = f(expr), ty = f(ty))
  )
}

@FunctionalInterface
trait NonlocalOrLocalReturnF[Rec <: TermT[Rec], ThisTree <: NonlocalOrLocalReturnC[Rec]] {
  def newNonlocalOrLocalReturn(value: Rec, meta: OptionTermMeta): ThisTree
}

trait NonlocalOrLocalReturnC[Rec <: TermT[Rec]] extends StmtTermT[Rec] {
  override type ThisTree <: NonlocalOrLocalReturnC[Rec]
  def value: Rec
  def cons: NonlocalOrLocalReturnF[Rec, ThisTree]

  override def toTerm: NonlocalOrLocalReturn = NonlocalOrLocalReturn(value.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("return") <+> value.toDoc

  def cpy(value: Rec = value, meta: OptionTermMeta = meta): ThisTree =
    cons.newNonlocalOrLocalReturn(value, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(value = f(value))
  )
}

case class NonlocalOrLocalReturn(value: Term, meta: OptionTermMeta) extends StmtTerm with NonlocalOrLocalReturnC[Term] {
  override type ThisTree = NonlocalOrLocalReturn
  override def cons: NonlocalOrLocalReturnF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): NonlocalOrLocalReturn = thisOr(
    copy(value = f(value))
  )
}

trait TupleTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: TupleTypeC[Rec]
  def types: Vector[Rec]
  override def toTerm: TupleType = TupleType(types.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`, ",")(types)
  }

  def cpy(types: Vector[Rec] = types, meta: OptionTermMeta = meta): ThisTree =
    cons.apply(types, meta)

  def cons: TupleTypeF[Rec, ThisTree]

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(types = types.map(f))
  )
}

@FunctionalInterface
trait TupleTypeF[Rec <: TermT[Rec], ThisTree <: TupleTypeC[Rec]] {
  def apply(types: Vector[Rec], meta: OptionTermMeta): ThisTree
}

case class TupleType(types: Vector[Term], meta: OptionTermMeta) extends TypeTerm with TupleTypeC[Term] {
  override type ThisTree = TupleType
  override def cons: TupleTypeF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): TupleType = thisOr(
    copy(types = types.map(f))
  )
}

trait TupleTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: TupleTermC[Rec]
  def values: Vector[Rec]
  override def toTerm: TupleTerm = TupleTerm(values.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`(`, Docs.`)`, ",")(values)
  }

  def cpy(values: Vector[Rec] = values, meta: OptionTermMeta = meta): ThisTree =
    cons.apply(values, meta)

  def cons: TupleTermF[Rec, ThisTree]

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(values = values.map(f))
  )
}

@FunctionalInterface
trait TupleTermF[Rec <: TermT[Rec], ThisTree <: TupleTermC[Rec]] {
  def apply(values: Vector[Rec], meta: OptionTermMeta): ThisTree
}

case class TupleTerm(values: Vector[Term], meta: OptionTermMeta) extends WHNF with TupleTermC[Term] {
  override type ThisTree = TupleTerm
  override def cons: TupleTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): TupleTerm = thisOr(
    copy(values = values.map(f))
  )
}

@FunctionalInterface
trait BlockTermF[Rec <: TermT[Rec], ThisTree <: BlockTermC[Rec]] {
  def newBlockTerm(statements: Vector[StmtTermT[Rec]], result: Rec, meta: OptionTermMeta): ThisTree
}

trait BlockTermC[Rec <: TermT[Rec]] extends UnevalT[Rec] {
  override type ThisTree <: BlockTermC[Rec]
  def statements: Vector[StmtTermT[Rec]]
  def result: Rec
  def cons: BlockTermF[Rec, ThisTree]

  override def toTerm: BlockTerm = BlockTerm(statements.map(_.toTerm), result.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ";")(
      (statements.map(_.toDoc) :+ result.toDoc)
    )
  }

  def cpy(statements: Vector[StmtTermT[Rec]] = statements, result: Rec = result, meta: OptionTermMeta = meta): ThisTree =
    cons.newBlockTerm(statements, result, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(
      statements = statements.map(g),
      result = f(result)
    )
  )
}

case class BlockTerm(
    statements: Vector[StmtTerm],
    result: Term,
    meta: OptionTermMeta
) extends Uneval
    with BlockTermC[Term] derives ReadWriter {
  override type ThisTree = BlockTerm
  override def cons: BlockTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): BlockTerm = thisOr(
    copy(
      statements = statements.map(g),
      result = f(result)
    )
  )
}

@FunctionalInterface
trait AnnotationF[Rec <: TermT[Rec], ThisTree <: AnnotationC[Rec]] {
  def newAnnotation(term: Rec, ty: Option[Rec], effects: Option[EffectsM], meta: OptionTermMeta): ThisTree
}

trait AnnotationC[Rec <: TermT[Rec]] extends UnevalT[Rec] {
  override type ThisTree <: AnnotationC[Rec]
  def term: Rec
  def ty: Option[Rec]
  def effects: Option[EffectsM]
  def cons: AnnotationF[Rec, ThisTree]

  override def toTerm: Annotation = Annotation(term.toTerm, ty.map(_.toTerm), effects, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
    val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
    term.toDoc <> tyDoc <> effectsDoc
  }

  def cpy(term: Rec = term, ty: Option[Rec] = ty, effects: Option[EffectsM] = effects, meta: OptionTermMeta = meta): ThisTree =
    cons.newAnnotation(term, ty, effects, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(x => g(x).asInstanceOf[EffectsM])
    )
  )
}

case class Annotation(
    term: Term,
    ty: Option[Term],
    effects: Option[EffectsM],
    meta: OptionTermMeta
) extends Uneval
    with AnnotationC[Term] {
  override type ThisTree = Annotation
  override def cons: AnnotationF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): Annotation = thisOr(
    copy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(g)
    )
  )

  require(ty.nonEmpty || effects.nonEmpty)
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

trait FieldTermC[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: FieldTermC[Rec]
  def name: Name
  def ty: Rec
  def cons: FieldTermF[Rec, ThisTree]
  override def toTerm: FieldTerm = FieldTerm(name, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(name) <> Doc.text(": ") <> ty.toDoc

  def cpy(name: Name = name, ty: Rec = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.apply(name, ty, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(ty = f(ty))
  )
}

@FunctionalInterface
trait FieldTermF[Rec <: TermT[Rec], ThisTree <: FieldTermC[Rec]] {
  def apply(name: Name, ty: Rec, meta: OptionTermMeta): ThisTree
}

case class FieldTerm(
    name: Name,
    ty: Term,
    meta: OptionTermMeta
) extends WHNF
    with FieldTermC[Term] derives ReadWriter {
  override type ThisTree = FieldTerm
  override def cons: FieldTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): FieldTerm = thisOr(
    copy(ty = f(ty))
  )
}

trait TypeDefinitionT[Rec <: TermT[Rec]] extends StmtTermT[Rec] with TermWithUniqidT[Rec] {
  @deprecated("dont use")
  def name: Name
  override type ThisTree <: TypeDefinitionT[Rec]
}

sealed trait TypeDefinition extends StmtTerm with TermWithUniqid with TypeDefinitionT[Term] derives ReadWriter {
  override type ThisTree <: TypeDefinition
  def uniqId: UniqidOf[TypeDefinition]

  override def switchUniqId(r: UReplacer): TypeDefinition
}

@FunctionalInterface
trait RecordStmtTermF[Rec <: TermT[Rec], ThisTree <: RecordStmtTermC[Rec]] {
  def newRecordStmt(
      name: Name,
      uniqId: UniqidOf[RecordStmtTermC[Rec]],
      fields: Vector[FieldTermC[Rec]],
      body: Option[BlockTermC[Rec]],
      meta: OptionTermMeta
  ): ThisTree
}

trait RecordStmtTermC[Rec <: TermT[Rec]] extends TypeDefinitionT[Rec] with StmtTermT[Rec] {
  override type ThisTree <: RecordStmtTermC[Rec]
  def name: Name
  def uniqId: UniqidOf[RecordStmtTermC[Rec]]
  def fields: Vector[FieldTermC[Rec]]
  def body: Option[BlockTermC[Rec]]
  def cons: RecordStmtTermF[Rec, ThisTree]
  override def toTerm: RecordStmtTerm =
    RecordStmtTerm(name, uniqId.asInstanceOf[UniqidOf[RecordStmtTerm]], fields.map(_.toTerm), body.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val fieldsDoc = fields.map(_.toDoc).reduceOption(_ <> Doc.text(", ") <> _).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).reduceOption(_ </> _).getOrElse(Doc.empty)

    group(
      Doc.text("record ") <> Doc.text(name) <>
        Doc.group(Doc.text("(") <> fieldsDoc <> Doc.text(")")) <>
        bodyDoc
    )
  }

  def cpy(
      name: Name = name,
      uniqId: UniqidOf[RecordStmtTermC[Rec]] = uniqId,
      fields: Vector[FieldTermC[Rec]] = fields,
      body: Option[BlockTermC[Rec]] = body,
      meta: OptionTermMeta = meta
  ): ThisTree =
    cons.newRecordStmt(name, uniqId, fields, body, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(
      fields = fields.map(g),
      body = body.map(g)
    )
  )

}

implicit def conversionRecord(x: UniqidOf[RecordStmtTermC[Term]]): UniqidOf[RecordStmtTerm] = x.asInstanceOf[UniqidOf[RecordStmtTerm]]
implicit def c1(x: Option[BlockTermC[Term]]): Option[BlockTerm] = x.asInstanceOf[Option[BlockTerm]]
case class RecordStmtTerm(
    name: Name,
    uniqId: UniqidOf[RecordStmtTerm] = Uniqid.generate[RecordStmtTerm],
    fields: Vector[FieldTerm],
    body: Option[BlockTerm],
    meta: OptionTermMeta
) extends TypeDefinition
    with RecordStmtTermC[Term] {
  override type ThisTree = RecordStmtTerm
  override def cons: RecordStmtTermF[Term, ThisTree] = this.copy

  override def switchUniqId(r: UReplacer): RecordStmtTerm = copy(uniqId = r(uniqId))
  override def descent(f: Term => Term, g: TreeMap[Term]): RecordStmtTerm = thisOr(
    copy(
      fields = fields.map(g),
      body = body.map(g)
    )
  )
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

@FunctionalInterface
trait ObjectCallTermF[Rec <: TermT[Rec], ThisTree <: ObjectCallTermC[Rec]] {
  def newObjectCallTerm(objectRef: Rec, meta: OptionTermMeta): ThisTree
}

trait ObjectCallTermC[Rec <: TermT[Rec]] extends UnevalT[Rec] {
  override type ThisTree <: ObjectCallTermC[Rec]
  def objectRef: Rec
  def cons: ObjectCallTermF[Rec, ThisTree]

  override def toTerm: ObjectCallTerm = ObjectCallTerm(objectRef.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  def cpy(objectRef: Rec = objectRef, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectCallTerm(objectRef, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(objectRef = f(objectRef))
  )
}

case class ObjectCallTerm(
    objectRef: Term,
    meta: OptionTermMeta
) extends Uneval
    with ObjectCallTermC[Term] {
  override type ThisTree = ObjectCallTerm
  override def cons: ObjectCallTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectCallTerm = thisOr(
    copy(objectRef = f(objectRef))
  )
}

@FunctionalInterface
trait ObjectTypeTermF[Rec <: TermT[Rec], ThisTree <: ObjectTypeTermC[Rec]] {
  def newObjectTypeTerm(objectDef: ObjectStmtTerm, meta: OptionTermMeta): ThisTree
}

trait ObjectTypeTermC[Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: ObjectTypeTermC[Rec]
  def objectDef: ObjectStmtTerm
  def cons: ObjectTypeTermF[Rec, ThisTree]

  override def toTerm: ObjectTypeTerm = ObjectTypeTerm(objectDef, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

  def cpy(objectDef: ObjectStmtTerm = objectDef, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectTypeTerm(objectDef, meta)

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = thisOr(
    cpy(objectDef = g(objectDef))
  )
}

case class ObjectTypeTerm(
    objectDef: ObjectStmtTerm,
    meta: OptionTermMeta
) extends TypeTerm
    with ObjectTypeTermC[Term] {
  override type ThisTree = ObjectTypeTerm
  override def cons: ObjectTypeTermF[Term, ThisTree] = this.copy

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectTypeTerm = thisOr(
    copy(objectDef = g(objectDef))
  )
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
sealed trait NodeKind derives ReadWriter
sealed trait AbstractNodeKind extends NodeKind derives ReadWriter
sealed trait ConcreteNodeKind extends NodeKind derives ReadWriter

object NodeKind {
  // AbstractNodeKinds
  case object Term extends AbstractNodeKind
  case object WHNF extends AbstractNodeKind
  case object Uneval extends AbstractNodeKind
  case object SpecialTerm extends AbstractNodeKind
  case object TermWithUniqid extends AbstractNodeKind
  case object EffectsM extends AbstractNodeKind
  case object Pat extends AbstractNodeKind
  case object TypeTerm extends AbstractNodeKind
  case object Sort extends AbstractNodeKind
  case object Level extends AbstractNodeKind
  case object LiteralTerm extends AbstractNodeKind
  case object AbstractIntTerm extends AbstractNodeKind
  case object WithType extends AbstractNodeKind
  case object ReferenceCall extends AbstractNodeKind
  case object ErrorTerm extends AbstractNodeKind
  case object StmtTerm extends AbstractNodeKind
  case object TypeDefinition extends AbstractNodeKind

  // ConcreteNodeKinds
  case object MetaTerm extends ConcreteNodeKind
  case object ListTerm extends ConcreteNodeKind
  case object Type extends ConcreteNodeKind
  case object Prop extends ConcreteNodeKind
  case object LevelFinite extends ConcreteNodeKind
  case object LevelUnrestricted extends ConcreteNodeKind
  case object IntTerm extends ConcreteNodeKind
  case object IntegerTerm extends ConcreteNodeKind
  case object RationalTerm extends ConcreteNodeKind
  case object BooleanTerm extends ConcreteNodeKind
  case object StringTerm extends ConcreteNodeKind
  case object SymbolTerm extends ConcreteNodeKind
  case object IntegerType extends ConcreteNodeKind
  case object IntType extends ConcreteNodeKind
  case object UIntType extends ConcreteNodeKind
  case object NaturalType extends ConcreteNodeKind
  case object RationalType extends ConcreteNodeKind
  case object FloatType extends ConcreteNodeKind
  case object StringType extends ConcreteNodeKind
  case object SymbolType extends ConcreteNodeKind
  case object AnyType extends ConcreteNodeKind
  case object LiteralType extends ConcreteNodeKind
  case object BooleanType extends ConcreteNodeKind
  case object LevelType extends ConcreteNodeKind
  case object ArgTerm extends ConcreteNodeKind
  case object TelescopeTerm extends ConcreteNodeKind
  case object Function extends ConcreteNodeKind
  case object FunctionType extends ConcreteNodeKind
  case object ObjectClauseValueTerm extends ConcreteNodeKind
  case object ObjectTerm extends ConcreteNodeKind
  case object ObjectType extends ConcreteNodeKind
  case object Union extends ConcreteNodeKind
  case object Intersection extends ConcreteNodeKind
  case object ExceptionEffect extends ConcreteNodeKind
  case object DivergeEffect extends ConcreteNodeKind
  case object IOEffect extends ConcreteNodeKind
  case object STEffect extends ConcreteNodeKind
  case object LocalV extends ConcreteNodeKind
  case object ToplevelV extends ConcreteNodeKind
  case object LetStmtTerm extends ConcreteNodeKind
  case object DefStmtTerm extends ConcreteNodeKind
  case object ExprStmtTerm extends ConcreteNodeKind
  case object NonlocalOrLocalReturn extends ConcreteNodeKind
  case object TupleType extends ConcreteNodeKind
  case object TupleTerm extends ConcreteNodeKind
  case object BlockTerm extends ConcreteNodeKind
  case object Annotation extends ConcreteNodeKind
  case object FieldTerm extends ConcreteNodeKind
  case object RecordStmtTerm extends ConcreteNodeKind
  case object RecordConstructorCallTerm extends ConcreteNodeKind
  case object TraitStmtTerm extends ConcreteNodeKind
  case object InterfaceStmtTerm extends ConcreteNodeKind
  case object ObjectCallTerm extends ConcreteNodeKind
  case object ObjectTypeTerm extends ConcreteNodeKind
  case object ObjectStmtTerm extends ConcreteNodeKind
}
