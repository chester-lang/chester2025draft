package chester.syntax.core

import chester.doc.*
import chester.error.*
import chester.error.ProblemUpickle.*
import chester.utils.{given}
import chester.utils.impls.*
import upickle.default.*

import scala.language.implicitConversions

import cats.data.*
import chester.doc.const.*
import chester.error.Problem
import chester.syntax.*
import chester.syntax.core.*
import chester.uniqid.*
import chester.utils.*
import chester.utils.doc.*
import spire.math.*

import scala.collection.immutable.HashMap

object simple {

  sealed trait Term extends TermT[Term] with ContainsUniqid derives ReadWriter {
    type ThisTree <: Term

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

  sealed trait Pat extends SpecialTerm with PatT[Term] derives ReadWriter {
    override type ThisTree <: Pat
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

  sealed trait WHNF extends Term with WHNFT[Term] derives ReadWriter {
    override type ThisTree <: WHNF
  }

  sealed trait Uneval extends Term with UnevalT[Term] derives ReadWriter {
    override type ThisTree <: Uneval
  }

  sealed trait SpecialTerm extends Term with SpecialTermT[Term] derives ReadWriter {
    override type ThisTree <: SpecialTerm
  }

  sealed trait TermWithUniqid extends Term with TermWithUniqidT[Term] derives ReadWriter {
    override type ThisTree <: TermWithUniqid

    override def uniqId: UniqidOf[Term]

    def switchUniqId(r: UReplacer): TermWithUniqid
  }

  sealed trait EffectsM extends Term with EffectsMT[Term] derives ReadWriter {
    override type ThisTree <: EffectsM
  }

  case class MetaTerm(impl: HoldNotReadable[?], meta: OptionTermMeta) extends Term with MetaTermC[Term] with EffectsM with SpecialTerm {
    override type ThisTree = MetaTerm
  }

  object MetaTerm {
    @deprecated("meta")
    def from[T](x: T): MetaTerm = MetaTerm(HoldNotReadable(x), meta = None)
  }

  case class ListTerm(terms: Vector[Term], meta: OptionTermMeta) extends Term with ListTermC[Term] with WHNF derives ReadWriter {
    override final type ThisTree = ListTerm

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist(Docs.`[`, Docs.`]`, ",")(terms)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(terms = terms.map(f)))
  }

  sealed trait TypeTerm extends Term with TypeTermT[Term] with WHNF derives ReadWriter {
    override type ThisTree <: TypeTerm
  }

  sealed trait Sort extends TypeTerm with SortT[Term] derives ReadWriter {
    override type ThisTree <: Sort

    def level: Term
  }

  case class Type(level: Term, meta: OptionTermMeta) extends Sort with TypeT[Term] {
    override type ThisTree = Type

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(Vector(level))

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))
  }

  case class LevelType(meta: OptionTermMeta) extends TypeTerm with WithType with LevelTypeC[Term] {
    override type ThisTree = LevelType

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("LevelType")

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

    override def ty: Term = Type0
  }

  sealed trait Level extends Term with LevelT[Term] with WHNF derives ReadWriter {
    type ThisTree <: Level
  }

  case class LevelFinite(n: Term, meta: OptionTermMeta) extends Level with LevelFiniteC[Term] {
    override type ThisTree = LevelFinite

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Level(") <> n.toDoc <> Doc.text(")")

    override def descent(f: Term => Term, g: TreeMap[Term]): LevelFinite =
      thisOr(copy(n = f(n)))
  }

  case class LevelUnrestricted(meta: OptionTermMeta) extends Level with LevelUnrestrictedC[Term] {
    override type ThisTree = LevelUnrestricted

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Levelω")

    override def descent(f: Term => Term, g: TreeMap[Term]): LevelUnrestricted = this
  }

  // Define Level0 using LevelFinite
  val Level0: LevelFinite = LevelFinite(IntegerTerm(0, meta = None), meta = None)

  val Type0: Type = Type(Level0, meta = None)

  // Referencing Setω in Agda
  val Typeω: Type = Type(LevelUnrestricted(None), meta = None)

  case class Prop(level: Term, meta: OptionTermMeta) extends Sort with PropC[Term] {
    override type ThisTree = Prop

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist("Prop" <> Docs.`(`, Docs.`)`)(Vector(level))
  }

  // fibrant types
  case class FType(level: Term, meta: OptionTermMeta) extends Sort with FTypeC[Term] {
    override type ThisTree = FType
  }

  sealed trait LiteralTerm extends Term with LiteralTermT[Term] with WHNF derives ReadWriter {
    override type ThisTree <: LiteralTerm
  }

  sealed trait AbstractIntTerm extends LiteralTerm with AbstractIntTermT[Term] derives ReadWriter {
    override type ThisTree <: AbstractIntTerm
  }

  case class IntTerm(value: Int, meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntTermC[Term] derives ReadWriter {
    override type ThisTree = IntTerm

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text(value.toString, ColorProfile.literalColor)
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

  sealed trait WithType extends Term with WithTypeT[Term] derives ReadWriter {
    override type ThisTree <: WithType

    def ty: Term
  }

  case class IntegerType(meta: OptionTermMeta) extends TypeTerm with WithType with IntegerTypeC[Term] derives ReadWriter {
    override type ThisTree = IntegerType

    override def descent(f: Term => Term, g: TreeMap[Term]): IntegerType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Integer", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  // int of 64 bits or more
  case class IntType(meta: OptionTermMeta) extends TypeTerm with WithType with IntTypeC[Term] derives ReadWriter {
    override type ThisTree = IntType

    override def descent(f: Term => Term, g: TreeMap[Term]): IntType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Int", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  // unsigned int of 64 bits or more
  case class UIntType(meta: OptionTermMeta) extends TypeTerm with WithType with UIntTypeC[Term] derives ReadWriter {
    override type ThisTree = UIntType

    override def descent(f: Term => Term, g: TreeMap[Term]): UIntType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("UInt", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  case class NaturalType(meta: OptionTermMeta) extends TypeTerm with WithType with NaturalTypeC[Term] derives ReadWriter {
    override type ThisTree = NaturalType

    override def descent(f: Term => Term, g: TreeMap[Term]): NaturalType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Natural", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  case class RationalTerm(value: Rational, meta: OptionTermMeta) extends LiteralTerm with RationalTermC[Term] derives ReadWriter {
    override type ThisTree = RationalTerm

    override def descent(f: Term => Term, g: TreeMap[Term]): RationalTerm = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text(value.toString, ColorProfile.literalColor)
  }

  case class BooleanTerm(value: Boolean, meta: OptionTermMeta) extends LiteralTerm with BooleanTermC[Term] derives ReadWriter {
    override type ThisTree = BooleanTerm

    override def descent(f: Term => Term, g: TreeMap[Term]): BooleanTerm = this

  }

  case class BooleanType(meta: OptionTermMeta) extends TypeTerm with WithType with BooleanTypeC[Term] derives ReadWriter {
    override type ThisTree = BooleanType

    override def descent(f: Term => Term, g: TreeMap[Term]): BooleanType = this

    override def ty: Term = Type0

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Boolean", ColorProfile.typeColor)
  }

  case class StringTerm(value: String, meta: OptionTermMeta) extends LiteralTerm with StringTermC[Term] derives ReadWriter {
    override type ThisTree = StringTerm

    override def descent(f: Term => Term, g: TreeMap[Term]): StringTerm = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
  }

  case class SymbolTerm(value: String, meta: OptionTermMeta) extends LiteralTerm with SymbolTermC[Term] derives ReadWriter {
    override type ThisTree = SymbolTerm

    override def descent(f: Term => Term, g: TreeMap[Term]): SymbolTerm = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("'" + value, ColorProfile.literalColor)
  }

  case class RationalType(meta: OptionTermMeta) extends TypeTerm with WithType with RationalTypeC[Term] derives ReadWriter {
    override type ThisTree = RationalType

    override def descent(f: Term => Term, g: TreeMap[Term]): RationalType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Rational", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  // float of 32 bits or more
  case class FloatType(meta: OptionTermMeta) extends TypeTerm with WithType with FloatTypeC[Term] derives ReadWriter {
    override type ThisTree = FloatType

    override def descent(f: Term => Term, g: TreeMap[Term]): FloatType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Float", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  case class StringType(meta: OptionTermMeta) extends TypeTerm with WithType with StringTypeC[Term] derives ReadWriter {
    override type ThisTree = StringType

    override def descent(f: Term => Term, g: TreeMap[Term]): StringType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("String", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

  case class SymbolType(meta: OptionTermMeta) extends TypeTerm with WithType with SymbolTypeC[Term] derives ReadWriter {
    override type ThisTree = SymbolType

    override def descent(f: Term => Term, g: TreeMap[Term]): SymbolType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Symbol", ColorProfile.typeColor)

    override def ty: Term = Type0
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

  def AnyType0: AnyType = AnyType(Level0, meta = None)

  val AnyType0Debug: AnyType = AnyType(Level0, meta = None)

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

  sealed trait Builtin extends WHNF derives ReadWriter {
    override type ThisTree <: Builtin
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

  sealed trait ReferenceCall extends Term with Uneval with TermWithUniqid with ReferenceCallC[Term] derives ReadWriter {
    override type ThisTree <: ReferenceCall
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

  case class ErrorTerm(problem: Problem, meta: OptionTermMeta) extends SpecialTerm with ErrorTermC[Term] {
    override type ThisTree = ErrorTerm
  }

  def ErrorType(error: Problem, meta: OptionTermMeta): ErrorTerm =
    ErrorTerm(error, meta)

  sealed trait StmtTerm extends Term with StmtTermT[Term] derives ReadWriter {
    override type ThisTree <: StmtTerm
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

  case class NonlocalOrLocalReturn(value: Term, meta: OptionTermMeta) extends StmtTerm with NonlocalOrLocalReturnC[Term] {
    override type ThisTree = NonlocalOrLocalReturn

    override def cons: NonlocalOrLocalReturnF[Term, ThisTree] = this.copy

    override def descent(f: Term => Term, g: TreeMap[Term]): NonlocalOrLocalReturn = thisOr(
      copy(value = f(value))
    )
  }

  case class TupleType(types: Vector[Term], meta: OptionTermMeta) extends TypeTerm with TupleTypeC[Term] {
    override type ThisTree = TupleType

    override def cons: TupleTypeF[Term, ThisTree] = this.copy

    override def descent(f: Term => Term, g: TreeMap[Term]): TupleType = thisOr(
      copy(types = types.map(f))
    )
  }

  case class TupleTerm(values: Vector[Term], meta: OptionTermMeta) extends WHNF with TupleTermC[Term] {
    override type ThisTree = TupleTerm

    override def cons: TupleTermF[Term, ThisTree] = this.copy

    override def descent(f: Term => Term, g: TreeMap[Term]): TupleTerm = thisOr(
      copy(values = values.map(f))
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

  def UnitType(meta: OptionTermMeta): TupleType =
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
  ) extends WHNF
      with FieldTermC[Term] derives ReadWriter {
    override type ThisTree = FieldTerm

    override def cons: FieldTermF[Term, ThisTree] = this.copy

    override def descent(f: Term => Term, g: TreeMap[Term]): FieldTerm = thisOr(
      copy(ty = f(ty))
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

  sealed trait TypeDefinition extends StmtTerm with TermWithUniqid with TypeDefinitionT[Term] derives ReadWriter {
    override type ThisTree <: TypeDefinition

    def uniqId: UniqidOf[TypeDefinition]

    override def switchUniqId(r: UReplacer): TypeDefinition
  }

  case class NothingType(meta: OptionTermMeta) extends TypeTerm with WithType with NothingTypeC[Term] {
    override type ThisTree = NothingType

    override def descent(f: Term => Term, g: TreeMap[Term]): NothingType = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Nothing", ColorProfile.typeColor)

    override def ty: Term = Type0
  }

}
