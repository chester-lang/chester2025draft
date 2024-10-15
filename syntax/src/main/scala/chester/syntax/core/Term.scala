// TODO: More correctly implement toDoc
package chester.syntax.core

import cats.data.*
import chester.doc.*
import chester.doc.const.{ColorProfile, Docs}
import chester.error.*
import chester.error.ProblemUpickle.*
import chester.syntax.{AbsoluteRef, Name}
import chester.utils.*
import chester.utils.doc.*
import chester.utils.impls.*
import spire.math.Rational
import upickle.default.*
import chester.uniqid.*
import scala.collection.immutable.HashMap
import scala.language.implicitConversions

case class TermMeta(sourcePos: SourcePos) derives ReadWriter

type OptionTermMeta = Option[TermMeta]

/** CallTerm has meta to trace runtime errors and debug */
sealed trait MaybeCallTerm extends Term derives ReadWriter {
  override def whnf: Boolean = false
}

case class CallingArgTerm(
    value: Term,
    ty: Term,
    name: Option[Name] = None,
    vararg: Boolean = false,
    meta: OptionTermMeta = None
) extends ToDoc derives ReadWriter {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

  def descent(f: Term => Term): CallingArgTerm =
    copy(value = f(value), ty = f(ty))
}

case class Calling(
    args: Vector[CallingArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta = None
) extends ToDoc derives ReadWriter {
  def toDoc(implicit options: PrettierOptions): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  def descent(f: Term => Term): Calling = copy(args = args.map(_.descent(f)))
}

case class FCallTerm(
    f: Term,
    args: Vector[Calling],
    meta: OptionTermMeta = None
) extends MaybeCallTerm {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

  override def whnf: Boolean = f match {
    case _ => false
  }

  override def descent(op: Term => Term): FCallTerm = thisOr(
    copy(f = f.descent(op), args = args.map(_.descent(op)))
  )
}

object FCallTerm {}

sealed trait Pat extends ToDoc derives ReadWriter {
  override def toDoc(implicit options: PrettierOptions): Doc = toString

  def descent(patOp: Pat => Pat, termOp: Term => Term): Pat = this

  def thisOr(x: Pat): this.type = reuse(this, x.asInstanceOf[this.type])
}

case class Bind(bind: LocalV, ty: Term, meta: OptionTermMeta = None) extends Pat {
  override def descent(patOp: Pat => Pat, termOp: Term => Term): Pat = thisOr(
    copy(ty = termOp(ty))
  )
}

object Bind {
  def from(bind: LocalV): Bind = Bind(bind, bind.ty)
}

sealed trait Term extends ToDoc with ContainsUniqId derives ReadWriter {
  def meta: OptionTermMeta

  def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)

  override def toDoc(implicit options: PrettierOptions): Doc = toString

  def whnf: Boolean = true

  protected final inline def thisOr[T <: Term](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  /** not type safe. unsound. Sometimes we have problems deciding how to do when a case class contains a subtype of Term */
  def descent(f: Term => Term): Term

  final def descentRecursive(f: Term => Term): Term = thisOr {
    f(descent(_.descentRecursive(f)))
  }

  def inspect(f: Term => Unit): Unit = {
    descent { x =>
      f(x)
      x
    }
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
      case _              => descent(_.replaceMeta(f))
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

sealed trait TermWithUniqId extends Term with HasUniqId derives ReadWriter {
  override def uniqId: UniqIdOf[Term]
  def switchUniqId(r: UReplacer): TermWithUniqId
}

// allow to write, not allow read
given MetaTermHoldRW: ReadWriter[MetaTermHold[?]] =
  readwriter[MetaTermRW].bimap(
    _ => MetaTermRW(),
    _ => {
      throw new UnsupportedOperationException("Cannot read MetaTerm")
    }
  )

case class MetaTermHold[T](inner: T) extends AnyVal

case class MetaTermRW() derives ReadWriter

case class MetaTerm(impl: MetaTermHold[?], meta: OptionTermMeta = None) extends Term {
  def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.group("Meta" <> Doc.text(impl.toString))

  override def descent(f: Term => Term): MetaTerm = this
}

object MetaTerm {
  def from[T](x: T): MetaTerm = MetaTerm(MetaTermHold(x))
}

case class ListTerm(terms: Vector[Term], meta: OptionTermMeta = None) extends Term derives ReadWriter {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(terms)

  override def descent(f: Term => Term): Term = thisOr(ListTerm(terms.map(f)))
}

object ListTerm {
  def apply(terms: Vector[Term]): ListTerm = new ListTerm(terms)

  def apply(terms: Seq[Term]): ListTerm = new ListTerm(terms.toVector)
}

sealed trait Sort extends Term derives ReadWriter {
  def level: Term
}

case class Type(level: Term, meta: OptionTermMeta = None) extends Sort with Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(Vector(level))

  override def descent(f: Term => Term): Term = thisOr(Type(f(level)))
}

case class LevelType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("LevelType")

  override def descent(f: Term => Term): Term = this

  override def ty: Term = Type0
}

sealed trait Level extends Term derives ReadWriter

case class LevelFinite(n: Term, meta: OptionTermMeta = None) extends Level {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Level(") <> n.toDoc <> Doc.text(")")

  override def descent(f: Term => Term): LevelFinite =
    thisOr(LevelFinite(f(n)))
}

case class Levelω(meta: OptionTermMeta = None) extends Level {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Levelω")

  override def descent(f: Term => Term): Levelω = this
}

// Define Level0 using LevelFinite
val Level0 = LevelFinite(IntegerTerm(0))

val Type0 = Type(Level0)

// Referencing Setω in Agda
val Typeω = Type(Levelω())

case class Prop(level: Term, meta: OptionTermMeta = None) extends Sort with Term {
  override def descent(f: Term => Term): Term = thisOr(Prop(f(level)))

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist("Prop" <> Docs.`(`, Docs.`)`)(Vector(level))
}

// fibrant types
case class FType(level: Term, meta: OptionTermMeta = None) extends Sort with Term {
  override def descent(f: Term => Term): Term = thisOr(FType(f(level)))

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
}

sealed trait LiteralTerm extends Term derives ReadWriter

case class IntTerm(value: Int, meta: OptionTermMeta = None) extends LiteralTerm derives ReadWriter {
  override def descent(f: Term => Term): IntTerm = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

case class IntegerTerm(value: BigInt, meta: OptionTermMeta = None) extends LiteralTerm derives ReadWriter {
  override def descent(f: Term => Term): IntegerTerm = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

type AbstractIntTerm = IntegerTerm | IntTerm

object AbstractIntTerm {
  def from(value: BigInt, meta: OptionTermMeta = None): AbstractIntTerm =
    if (value.isValidInt) IntTerm(value.toInt, meta)
    else IntegerTerm(value, meta)

  def unapply(term: Term): Option[BigInt] = term match {
    case IntTerm(value, _)     => Some(BigInt(value))
    case IntegerTerm(value, _) => Some(value)
    case _                     => None
  }
}

object NaturalTerm {
  def apply(value: BigInt): AbstractIntTerm = AbstractIntTerm.from(value)
}

sealed trait TypeTerm extends Term derives ReadWriter {}

sealed trait WithType extends Term derives ReadWriter {
  def ty: Term
}

case class IntegerType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): IntegerType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Integer", ColorProfile.typeColor)

  override def ty: Term = Type0
}

// int of 64 bits or more
case class IntType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): IntType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Int", ColorProfile.typeColor)

  override def ty: Term = Type0
}

// unsigned int of 64 bits or more
case class UIntType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): UIntType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("UInt", ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class NaturalType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): NaturalType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class RationalTerm(value: Rational, meta: OptionTermMeta = None) extends LiteralTerm derives ReadWriter {
  override def descent(f: Term => Term): RationalTerm = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

case class StringTerm(value: String, meta: OptionTermMeta = None) extends LiteralTerm derives ReadWriter {
  override def descent(f: Term => Term): StringTerm = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
}

case class SymbolTerm(value: String, meta: OptionTermMeta = None) extends Term derives ReadWriter {
  override def descent(f: Term => Term): SymbolTerm = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("'" + value, ColorProfile.literalColor)
}

case class RationalType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): RationalType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Rational", ColorProfile.typeColor)

  override def ty: Term = Type0
}

// float of 32 bits or more
case class FloatType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): FloatType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Float", ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class StringType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): StringType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("String", ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class SymbolType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): SymbolType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Symbol", ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class AnyType(level: Term, meta: OptionTermMeta = None) extends TypeTerm with WithType derives ReadWriter {
  override def descent(f: Term => Term): AnyType = thisOr(
    copy(level = f(level))
  )

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Any", ColorProfile.typeColor)

  override def ty: Term = Type(level)
}

def AnyType0 = AnyType(Level0)

val AnyType0Debug = AnyType(Level0)

case class NothingType(meta: OptionTermMeta = None) extends TypeTerm with WithType {
  override def descent(f: Term => Term): NothingType = this

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("Nothing", ColorProfile.typeColor)

  override def ty: Term = Type0
}

implicit val rwUnionHere: ReadWriter[IntegerTerm | SymbolTerm | StringTerm | RationalTerm] =
  union4RW[IntegerTerm, SymbolTerm, StringTerm, RationalTerm]

case class LiteralType(
    literal: IntegerTerm | SymbolTerm | StringTerm | RationalTerm,
    meta: OptionTermMeta = None
) extends TypeTerm
    with WithType {
  override def descent(f: Term => Term): LiteralType = copy(literal =
    literal
      .descent(f)
      .asInstanceOf[IntegerTerm | SymbolTerm | StringTerm | RationalTerm]
  )

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text(literal.toString, ColorProfile.typeColor)

  override def ty: Term = Type0
}

case class ArgTerm(
    bind: LocalV,
    ty: Term,
    default: Option[Term] = None,
    vararg: Boolean = false,
    meta: OptionTermMeta = None
) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
    bind.toDoc <> varargDoc <> Docs.`:` <+> ty.toDoc <> defaultDoc
  }

  def name: Name = bind.name

  override def descent(f: Term => Term): ArgTerm = thisOr(
    copy(bind = f(bind).asInstanceOf[LocalV], ty = f(ty), default = default.map(f))
  )
}

object ArgTerm {
  def from(bind: LocalV): ArgTerm = ArgTerm(bind, bind.ty)
}

object TelescopeTerm {
  def from(x: ArgTerm*): TelescopeTerm = TelescopeTerm(x.toVector)
}

case class TelescopeTerm(
    args: Vector[ArgTerm],
    implicitly: Boolean = false,
    meta: OptionTermMeta = None
) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val argsDoc =
      args.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    if (implicitly) {
      Docs.`[` <> argsDoc <> Docs.`]`
    } else {
      Docs.`(` <> argsDoc <> Docs.`)`
    }
  }

  override def descent(f: Term => Term): TelescopeTerm = thisOr(
    copy(args = args.map(_.descent(f)))
  )
}

case class Function(ty: FunctionType, body: Term, meta: OptionTermMeta = None) extends Term {

  override def toDoc(implicit options: PrettierOptions): Doc = {
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

  override def descent(f: Term => Term): Term = thisOr(
    copy(
      ty = ty.descent(f),
      body = f(body)
    )
  )
}

case class MatchingClause() derives ReadWriter {}

case class Matching(
    ty: FunctionType,
    clauses: NonEmptyVector[MatchingClause],
    meta: OptionTermMeta = None
) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = toString // TODO

  override def descent(f: Term => Term): Term = thisOr(copy(ty = ty.descent(f)))
}

// Note that effect and result can use variables from telescope
case class FunctionType(
    telescope: Vector[TelescopeTerm],
    resultTy: Term,
    effects: EffectsM = NoEffect,
    meta: OptionTermMeta = None
) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val telescopeDoc =
      telescope.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val effectsDoc = if (effects.nonEmpty) {
      Docs.`/` <+> effects.toDoc
    } else {
      Doc.empty
    }
    group(telescopeDoc <+> Docs.`->` <+> resultTy.toDoc <> effectsDoc)
  }

  override def descent(f: Term => Term): FunctionType = thisOr(
    copy(
      telescope = telescope.map(_.descent(f)),
      effects = effects.descentM(f),
      resultTy = f(resultTy)
    )
  )
}

object FunctionType {
  def apply(
      telescope: Vector[TelescopeTerm],
      resultTy: Term,
      effects: EffectsM = NoEffect,
      meta: OptionTermMeta = None
  ): FunctionType = {
    new FunctionType(telescope, resultTy, effects, meta)
  }

  def apply(telescope: TelescopeTerm, resultTy: Term): FunctionType = {
    new FunctionType(Vector(telescope), resultTy)
  }
}

def TyToty: FunctionType = {
  val ty = LocalV("x", Type0, UniqId.generate[LocalV])
  FunctionType(TelescopeTerm.from(ArgTerm.from(ty)), ty)
}

case class ObjectClauseValueTerm(
    key: Term,
    value: Term,
    meta: OptionTermMeta = None
) derives ReadWriter {
  def toDoc(implicit options: PrettierOptions): Doc = group(
    key <+> Doc.text("=") <+> value
  )

  def descent(f: Term => Term): ObjectClauseValueTerm =
    (copy(key = f(key), value = f(value)))
}

case class ObjectTerm(
    clauses: Vector[ObjectClauseValueTerm],
    meta: OptionTermMeta = None
) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc))

  override def descent(f: Term => Term): ObjectTerm = thisOr(
    copy(clauses = clauses.map(_.descent(f)))
  )
}

// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
case class ObjectType(
    fieldTypes: Vector[ObjectClauseValueTerm],
    exactFields: Boolean = false,
    meta: OptionTermMeta = None
) extends Term {
  override def descent(f: Term => Term): ObjectType = thisOr(
    copy(fieldTypes = fieldTypes.map(_.descent(f)))
  )

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(
      fieldTypes.map(_.toDoc)
    )
}

sealed trait Builtin extends Term derives ReadWriter

case class ListF(meta: OptionTermMeta = None) extends Builtin {
  override def descent(f: Term => Term): ListF = this

  override def toDoc(implicit options: PrettierOptions): Doc = "List"
}

sealed trait Constructed extends Term derives ReadWriter

case class ListType(ty: Term, meta: OptionTermMeta = None) extends Constructed with TypeTerm {
  override def descent(f: Term => Term): ListType = thisOr(copy(ty = f(ty)))

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("List") <> Docs.`(` <> ty <> Docs.`)`
}

case class Union(xs: NonEmptyVector[Term], meta: OptionTermMeta = None) extends TypeTerm {
  override def descent(f: Term => Term): Union = thisOr(copy(xs = xs.map(f)))

  override def toDoc(implicit options: PrettierOptions): Doc =
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
    if (flattened.nonEmpty) new Union(flattened.assumeNonEmpty)
    else NothingType()
  }
}

case class Intersection(xs: NonEmptyVector[Term], meta: OptionTermMeta = None) extends TypeTerm derives ReadWriter {
  override def descent(f: Term => Term): Intersection = thisOr(
    copy(xs = xs.map(f))
  )

  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, " & ")(xs)
}

object Intersection {
  // def apply(xs: Vector[Term]): AndType = flatList[AndType]((x => new AndType(x)), { case AndType(x) => Some(x); case _ => None })(xs)
  def from(xs: Vector[Term]): Term = {
    val flattened = xs.flatMap {
      case Intersection(ys, _) => ys
      case x                   => Vector(x)
    }.distinct
    if (flattened.size == 1) return flattened.head
    new Intersection(flattened.assumeNonEmpty)
  }
}

/** Effect needs to have reasonable equals and hashcode for simple comparison, whereas they are not requirements for other Terms
  */
sealed trait Effect extends Term derives ReadWriter {
  def name: String

  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text(name)

  override def descent(f: Term => Term): Effect = this
}

type OrM[T <: Term] = (T | MetaTerm)

type EffectsM = OrM[Effects]

given OrMRW[T <: Term]: ReadWriter[OrM[T]] =
  readwriter[Term].asInstanceOf[ReadWriter[OrM[T]]]

extension (e: EffectsM) {
  def descentM(f: Term => Term): EffectsM = e match {
    case e: Effects  => e.descent(f)
    case e: MetaTerm => f(e).asInstanceOf[EffectsM]
  }
  def nonEmpty: Boolean = e match {
    case e: Effects => e.nonEmpty
    case _          => true
  }
}

case class Effects(effects: Map[LocalV, Term], meta: OptionTermMeta = None) extends ToDoc with Term derives ReadWriter {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(effects.map { case (k, v) =>
      k.toDoc <+> Docs.`:` <+> v.toDoc
    })

  def descent(f: Term => Term): Effects = Effects(effects.map { case (effect, names) =>
    f(effect).asInstanceOf[LocalV] -> f(names)
  })

  def isEmpty: Boolean = (effects eq NoEffect.effects) || effects.isEmpty

  def nonEmpty: Boolean = (effects ne NoEffect.effects) && effects.nonEmpty

  override def collectMeta: Vector[MetaTerm] =
    effects.flatMap((a, b) => a.collectMeta ++ b.collectMeta).toVector

  override def replaceMeta(f: MetaTerm => Term): Effects = Effects(effects.map { case (a, b) =>
    a.replaceMeta(f).asInstanceOf[LocalV] -> b.replaceMeta(f)
  })
}

object Effects {
  val Empty: Effects = Effects(HashMap.empty)

}

val NoEffect = Effects.Empty

// may raise an exception
case class ExceptionEffect(meta: OptionTermMeta = None) extends Effect {
  val name = "Exception"
}

// may not terminate
case class DivergeEffect(meta: OptionTermMeta = None) extends Effect {
  val name = "Diverge"
}

// whatever IO: console, file, network, ...
case class IOEffect(meta: OptionTermMeta = None) extends Effect {
  val name = "IO"
}

case class STEffect(meta: OptionTermMeta = None) extends Effect {
  val name = "ST"
}

sealed trait MaybeVarCall extends MaybeCallTerm with TermWithUniqId derives ReadWriter {
  def name: Name
  def ty: Term
}

case class LocalV(
    name: Name,
    ty: Term,
    uniqId: UniqIdOf[LocalV],
    meta: OptionTermMeta = None
) extends MaybeVarCall derives ReadWriter {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text(name.toString)

  override def descent(f: Term => Term): LocalV = thisOr(copy(ty = f(ty)))

  override def switchUniqId(r: UReplacer): TermWithUniqId = copy(uniqId = r(uniqId))
}

case class ToplevelV(
    id: AbsoluteRef,
    ty: Term,
    uniqId: UniqIdOf[ToplevelV],
    meta: OptionTermMeta = None
) extends MaybeVarCall {
  override def name: Name = id.name

  override def toDoc(implicit options: PrettierOptions): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )

  override def descent(f: Term => Term): ToplevelV = thisOr(copy(ty = f(ty)))
  override def switchUniqId(r: UReplacer): TermWithUniqId = copy(uniqId = r(uniqId))
}

case class ErrorTerm(problem: Problem, meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = problem.toDoc

  override def descent(f: Term => Term): ErrorTerm = this
}

def ErrorType(error: Problem, meta: OptionTermMeta = None): ErrorTerm =
  ErrorTerm(error, meta)

sealed trait StmtTerm extends Term derives ReadWriter {
  def descent(f: Term => Term): StmtTerm = ???
}

case class LetStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta = None
) extends StmtTerm {
  override def descent(f: Term => Term): LetStmtTerm =
    copy(
      localv = localv.copy(ty = f(localv.ty)),
      value = f(value),
      ty = f(ty)
    )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }
}

case class DefStmtTerm(
    localv: LocalV,
    value: Term,
    ty: Term,
    meta: OptionTermMeta = None
) extends StmtTerm {
  override def descent(f: Term => Term): DefStmtTerm =
    copy(
      localv = localv.copy(ty = f(localv.ty)),
      value = f(value),
      ty = f(ty)
    )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }
}

case class ExprStmtTerm(
    expr: Term,
    ty: Term = AnyType0,
    meta: OptionTermMeta = None
) extends StmtTerm {
  override def descent(f: Term => Term): StmtTerm =
    copy(expr = f(expr), ty = f(ty))

  override def toDoc(implicit options: PrettierOptions): Doc = expr.toDoc
}

case class NonlocalOrLocalReturn(value: Term, meta: OptionTermMeta = None) extends StmtTerm {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("return") <+> value.toDoc
}

case class TupleType(types: Vector[Term], meta: OptionTermMeta = None) extends TypeTerm {
  override def descent(f: Term => Term): TupleType = thisOr(
    copy(types = types.map(f))
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`, ",")(types)
  }
}

case class TupleTerm(values: Vector[Term], meta: OptionTermMeta = None) extends Term {
  override def descent(f: Term => Term): TupleTerm = thisOr(
    copy(values = values.map(f))
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`(`, Docs.`)`, ",")(values)
  }
}

case class BlockTerm(
    stmts: Vector[StmtTerm],
    value: Term,
    meta: OptionTermMeta = None
) extends Term derives ReadWriter {
  override def descent(f: Term => Term): BlockTerm = thisOr(
    copy(
      stmts = stmts.map {
        _.descent(f)
      },
      value = f(value)
    )
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ";")(
      (stmts.map(_.toDoc) :+ value.toDoc)
    )
  }
}

object BlockTerm {
  def apply(stmts: Vector[StmtTerm], value: Term): BlockTerm =
    new BlockTerm(stmts, value)

  def apply(stmts: Seq[StmtTerm], value: Term): BlockTerm =
    new BlockTerm(stmts.toVector, value)
}

case class Annotation(
    term: Term,
    ty: Option[Term],
    effects: Option[EffectsM],
    meta: OptionTermMeta = None
) extends Term {
  require(ty.nonEmpty || effects.nonEmpty)

  override def descent(f: Term => Term): Annotation = thisOr(
    copy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(_.descentM(f))
    )
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
    val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
    term.toDoc <> tyDoc <> effectsDoc
  }
}

// TODO: tuple?
def UnitType(meta: OptionTermMeta = None) =
  ObjectType(Vector.empty, meta = meta)
def UnitTerm(meta: OptionTermMeta = None) =
  ObjectTerm(Vector.empty, meta = meta)

case class FieldTerm(
    name: Name,
    ty: Term,
    meta: OptionTermMeta = None
) extends Term derives ReadWriter {
  override def descent(f: Term => Term): Term = copy(ty = f(ty))

  override def toDoc(implicit options: PrettierOptions): Doc =
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
    meta: OptionTermMeta = None
) extends TypeDefinition {
  override def switchUniqId(r: UReplacer): RecordStmtTerm = copy(uniqId = r(uniqId))
  override def descent(f: Term => Term): RecordStmtTerm = copy(
    fields = fields.map(f).asInstanceOf[Vector[FieldTerm]],
    body = body.map(f).asInstanceOf[Option[BlockTerm]]
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
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
    meta: OptionTermMeta = None
) extends Term {
  override def descent(f: Term => Term): RecordConstructorCallTerm = copy(
    args = args.map(f)
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
    val argsDoc = Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,`)(args.map(_.toDoc))
    Doc.text(recordName) <> argsDoc
  }
}
case class TraitStmtTerm(
    name: Name,
    uniqId: UniqIdOf[TraitStmtTerm] = UniqId.generate[TraitStmtTerm],
    extendsClause: Option[Term] = None,
    body: Option[BlockTerm] = None,
    meta: OptionTermMeta = None
) extends TypeDefinition derives ReadWriter {
  override def switchUniqId(r: UReplacer): TraitStmtTerm = copy(uniqId = r(uniqId))

  override def descent(f: Term => Term): TraitStmtTerm = copy(
    extendsClause = extendsClause.map(f),
    body = body.map(_.descent(f).asInstanceOf[BlockTerm])
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
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
    meta: OptionTermMeta = None
) extends TypeDefinition derives ReadWriter {
  override def switchUniqId(r: UReplacer): InterfaceStmtTerm = copy(uniqId = r(uniqId))

  override def descent(f: Term => Term): InterfaceStmtTerm = copy(
    extendsClause = extendsClause.map(f),
    body = body.map(_.descent(f).asInstanceOf[BlockTerm])
  )

  override def toDoc(implicit options: PrettierOptions): Doc = {
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
    meta: OptionTermMeta = None
) extends TypeDefinition derives ReadWriter {
  override def switchUniqId(r: UReplacer): ObjectStmtTerm = copy(uniqId = r(uniqId))
  override def toDoc(implicit options: PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    Doc.text("object") <+> Doc.text(name) <+> extendsDoc <+> bodyDoc
  }

  override def descent(f: Term => Term): ObjectStmtTerm = thisOr(
    copy(
      extendsClause = extendsClause.map(f),
      body = body.map(_.descent(f))
    )
  )
}

case class ObjectCallTerm(
    objectRef: Term,
    meta: OptionTermMeta = None
) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  override def descent(f: Term => Term): ObjectCallTerm =
    copy(objectRef = f(objectRef))
}
case class ObjectTypeTerm(
    objectDef: ObjectStmtTerm,
    meta: OptionTermMeta = None
) extends TypeTerm {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

  override def descent(f: Term => Term): Term = this
}
case class TodoTerm(meta: OptionTermMeta = None) extends Term {
  override def toDoc(implicit options: PrettierOptions): Doc = Doc.text("TODO")
  override def descent(f: Term => Term): TodoTerm = this
}
