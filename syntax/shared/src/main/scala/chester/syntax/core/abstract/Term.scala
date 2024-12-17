// TODO: More correctly implement toDoc
package chester.syntax.core

import chester.doc.*
import chester.doc.const.{ColorProfile, Docs}
import chester.error.*
import chester.syntax.*
import chester.syntax.core.orm.*
import chester.uniqid.*
import chester.utils.*
import chester.utils.doc.*
import spire.math.Trilean.*
import spire.math.{Rational, Trilean}
import upickle.default.*

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
 * - TermT[Term] - Base trait for all terms
 *   - LocalVC[Term] - Structure for local variables
 *     - LocalV - Concrete implementation
 *   - LocalVF[Term, ThisTree] - Factory for creating LocalV instances
 */

case class TermMeta(sourcePos: SourcePos) derives ReadWriter

type OptionTermMeta = Option[TermMeta]

@FunctionalInterface
trait CallingArgTermF[Term <: TermT[Term], ThisTree <: CallingArgTermC[Term]] {
  def newCallingArgTerm(value: Term, ty: Term, name: Option[Name], vararg: Boolean, meta: OptionTermMeta): ThisTree
}

trait CallingArgTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: CallingArgTermC[Term]
  def value: Term
  def ty: Term
  def name: Option[Name]
  def vararg: Boolean
  override def toTerm: CallingArgTerm = CallingArgTerm(value.toTerm, ty.toTerm, name, vararg, meta)
  def cons: CallingArgTermF[Term, ThisTree]

  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

  def cpy(
      value: Term = value,
      ty: Term = ty,
      name: Option[Name] = name,
      vararg: Boolean = vararg,
      meta: OptionTermMeta = meta
  ): ThisTree =
    cons.newCallingArgTerm(value, ty, name, vararg, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(value = f(value), ty = f(ty))
  )
}

@FunctionalInterface
trait CallingF[Term <: TermT[Term], ThisTree <: CallingC[Term]] {
  def newCalling(args: Vector[CallingArgTermC[Term]], implicitly: Boolean, meta: OptionTermMeta): ThisTree
}

trait CallingC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: CallingC[Term]
  def args: Vector[CallingArgTermC[Term]]
  def implicitly: Boolean
  override def toTerm: Calling = Calling(args.map(_.toTerm), implicitly, meta)
  def cons: CallingF[Term, ThisTree]

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  def cpy(
      args: Vector[CallingArgTermC[Term]] = args,
      implicitly: Boolean = implicitly,
      meta: OptionTermMeta = meta
  ): ThisTree =
    cons.newCalling(args, implicitly, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(args = args.map(g))
  )
}

@FunctionalInterface
trait FCallTermF[Term <: TermT[Term], ThisTree <: FCallTermC[Term]] {
  def newFCallTerm(f: Term, args: Vector[CallingC[Term]], meta: OptionTermMeta): ThisTree
}

trait FCallTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: FCallTermC[Term]
  def f: Term
  def args: Vector[CallingC[Term]]
  override def toTerm: FCallTerm = FCallTerm(f.toTerm, args.map(_.toTerm), meta)
  def cons: FCallTermF[Term, ThisTree]

  override def toDoc(using options: PrettierOptions): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

  def cpy(f: Term = f, args: Vector[CallingC[Term]] = args, meta: OptionTermMeta = meta): ThisTree =
    cons.newFCallTerm(f, args, meta)

  def descent(a: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(f = a(f), args = args.map(g))
  )
}

trait PatT[Term <: TermT[Term]] extends SpecialTermT[Term] {
  override type ThisTree <: PatT[Term]
}

@FunctionalInterface
trait BindF[Term <: TermT[Term], ThisTree <: BindC[Term]] {
  def newBind(bind: LocalVC[Term], ty: Term, meta: OptionTermMeta): ThisTree
}

trait BindC[Term <: TermT[Term]] extends PatT[Term] {
  override type ThisTree <: BindC[Term]

  def bind: LocalVC[Term]
  def ty: Term
  def cons: BindF[Term, ThisTree]

  override def toTerm: Bind = Bind(bind.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = bind.toDoc <+> Docs.`:` <+> ty.toDoc

  def cpy(bind: LocalVC[Term] = bind, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree = cons.newBind(bind, ty, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(bind = g(bind), ty = f(ty)))
}

/** more abstract Term. sealed trait *T corresponds to sealed trait in Term; trait *C corresponds to case class in Term */
trait TermT[Term <: TermT[Term]] extends Any with ToDoc with Tree[Term] {
  override def toDoc(using options: PrettierOptions): Doc = toString

  def descent(f: Term => Term, g: TreeMap[Term]): Term

  def mapFlatten[B](f: Term => Seq[B]): Vector[B] = {
    var result = Vector.empty[B]
    inspectRecursive { term =>
      result ++= f(term)
    }
    result
  }

  def meta: OptionTermMeta
  def whnf: Trilean
  def toTerm: simple.Term = {
    assert(this.isInstanceOf[simple.Term], "forgot to implement toTerm?")
    this.asInstanceOf[simple.Term]
  }
  def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)
}

type AnyTerm = TermT[?]

trait WHNFT[Term <: TermT[Term]] extends TermT[Term] {
  override type ThisTree <: WHNFT[Term]
  override def whnf: Trilean = True
}

trait UnevalT[Term <: TermT[Term]] extends TermT[Term] {
  override type ThisTree <: UnevalT[Term]
  override def whnf: Trilean = False
}

trait SpecialTermT[Term <: TermT[Term]] extends TermT[Term] {
  override type ThisTree <: SpecialTermT[Term]
  override def whnf: Trilean = Unknown
}

trait TermWithUniqidT[Term <: TermT[Term]] extends TermT[Term] with HasUniqid {
  override type ThisTree <: TermWithUniqidT[Term]
  override def uniqId: UniqidOf[TermT[Term]]
}

trait EffectsMT[Term <: TermT[Term]] extends TermT[Term] {
  override type ThisTree <: EffectsMT[Term]
}

trait MetaTermC[Term <: TermT[Term]] extends TermT[Term] with EffectsMT[Term] with SpecialTermT[Term] {
  override type ThisTree <: MetaTermC[Term]
  def impl: HoldNotReadable[?]
  override def toTerm: MetaTerm = MetaTerm(impl, meta)
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.group("Meta" <> Doc.text(impl.toString))

  def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}

trait ListTermC[Term <: TermT[Term]] extends TermT[Term] with WHNFT[Term] {
  override type ThisTree <: ListTermC[Term]
  def terms: Vector[Term]
  override def toTerm: ListTerm = ListTerm(terms.map(_.toTerm), meta)
}

trait TypeTermT[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: TypeTermT[Term]
}

trait SortT[Term <: TermT[Term]] extends TypeTermT[Term] {
  override type ThisTree <: SortT[Term]
  def level: TermT[Term]
}

trait TypeT[Term <: TermT[Term]] extends SortT[Term] {
  override type ThisTree <: TypeT[Term]
  def level: Term
  override def toTerm: Type = Type(level.toTerm, meta)
}

trait LevelTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: LevelTypeC[Term]
  override def toTerm: LevelType = LevelType(meta)
}

trait LevelT[Term <: TermT[Term]] extends TypeTermT[Term] with WHNFT[Term] {
  override type ThisTree <: LevelT[Term]
}

trait LevelFiniteC[Term <: TermT[Term]] extends LevelT[Term] {
  override type ThisTree <: LevelFiniteC[Term]
  def n: Term
  override def toTerm: LevelFinite = LevelFinite(n.toTerm, meta)
}

trait LevelUnrestrictedC[Term <: TermT[Term]] extends LevelT[Term] {
  override type ThisTree <: LevelUnrestrictedC[Term]
  override def toTerm: LevelUnrestricted = LevelUnrestricted(meta)
}

enum Usage derives ReadWriter {
  case None, Linear, Unrestricted
}

trait PropC[Term <: TermT[Term]] extends SortT[Term] {
  override type ThisTree <: PropC[Term]
  def level: Term
  override def toTerm: Prop = Prop(level.toTerm, meta)
}

trait FTypeC[Term <: TermT[Term]] extends SortT[Term] {
  override type ThisTree <: FTypeC[Term]
  def level: Term
  def copy(level: Term = level, meta: OptionTermMeta = meta): ThisTree
  override def toTerm: FType = FType(level.toTerm, meta)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
}

trait LiteralTermT[Term <: TermT[Term]] extends TermT[Term] with WHNFT[Term] {
  override type ThisTree <: LiteralTermT[Term]
}

trait AbstractIntTermT[Term <: TermT[Term]] extends LiteralTermT[Term] {
  override type ThisTree <: AbstractIntTermT[Term]
}

trait IntTermC[Term <: TermT[Term]] extends LiteralTermT[Term] with AbstractIntTermT[Term] {
  override type ThisTree <: IntTermC[Term]
  def value: Int
  override def toTerm: IntTerm = IntTerm(value, meta)
}

trait IntegerTermC[Term <: TermT[Term]] extends LiteralTermT[Term] with AbstractIntTermT[Term] {
  override type ThisTree <: IntegerTermC[Term]
  def value: BigInt
  override def toTerm: IntegerTerm = IntegerTerm(value, meta)
}

trait WithTypeT[Term <: TermT[Term]] extends TermT[Term] {
  override type ThisTree <: WithTypeT[Term]
  def ty: Term
}

trait IntegerTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: IntegerTypeC[Term]
  override def toTerm: IntegerType = IntegerType(meta)
}

trait IntTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: IntTypeC[Term]
  override def toTerm: IntType = IntType(meta)
}

trait UIntTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: UIntTypeC[Term]
  override def toTerm: UIntType = UIntType(meta)
}

trait NaturalTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: NaturalTypeC[Term]
  override def toTerm: NaturalType = NaturalType(meta)
}

trait RationalTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
  override type ThisTree <: RationalTermC[Term]
  def value: Rational
  override def toTerm: RationalTerm = RationalTerm(value, meta)
}

trait BooleanTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
  override type ThisTree <: BooleanTermC[Term]
  def value: Boolean
  override def toTerm: BooleanTerm = BooleanTerm(value, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait BooleanTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: BooleanTypeC[Term]
  override def toTerm: BooleanType = BooleanType(meta)
}

trait StringTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
  override type ThisTree <: StringTermC[Term]
  def value: String
  override def toTerm: StringTerm = StringTerm(value, meta)
}

trait SymbolTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
  override type ThisTree <: SymbolTermC[Term]
  def value: String
  override def toTerm: SymbolTerm = SymbolTerm(value, meta)
}

trait RationalTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: RationalTypeC[Term]
  override def toTerm: RationalType = RationalType(meta)
}

trait FloatTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: FloatTypeC[Term]
  override def toTerm: FloatType = FloatType(meta)
}

trait StringTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: StringTypeC[Term]
  override def toTerm: StringType = StringType(meta)
}

trait SymbolTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: SymbolTypeC[Term]
  override def toTerm: SymbolType = SymbolType(meta)
}

trait AnyTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: AnyTypeC[Term]
  def level: Term
  override def toTerm: AnyType = AnyType(level.toTerm, meta)
}

trait NothingTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: NothingTypeC[Term]
  override def toTerm: NothingType = NothingType(meta)
}

trait LiteralTypeC[Term <: TermT[Term]] extends TypeTermT[Term] with WithTypeT[Term] {
  override type ThisTree <: LiteralTypeC[Term]
  def literal: LiteralTermT[Term]
  override def toTerm: LiteralType = LiteralType(literal.toTerm.asInstanceOf[LiteralTerm], meta)
}

@FunctionalInterface
trait ArgTermF[Term <: TermT[Term], ThisTree <: ArgTermC[Term]] {
  def newArgTerm(bind: LocalV, ty: Term, default: Option[Term], vararg: Boolean, meta: OptionTermMeta): ThisTree
}

trait ArgTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: ArgTermC[Term]

  def bind: LocalV
  def ty: Term
  def default: Option[Term]
  def vararg: Boolean
  def cons: ArgTermF[Term, ThisTree]

  override def toTerm: ArgTerm = ArgTerm(bind, ty.toTerm, default.map(_.toTerm), vararg, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
    bind.toDoc <> varargDoc <> Docs.`:` <+> ty.toDoc <> defaultDoc
  }

  def cpy(bind: LocalV = bind, ty: Term = ty, default: Option[Term] = default, vararg: Boolean = vararg, meta: OptionTermMeta = meta): ThisTree =
    cons.newArgTerm(bind, ty, default, vararg, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(bind = g(bind), ty = f(ty), default = default.map(f))
  )

  def name = bind.name
}

trait TelescopeTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: TelescopeTermC[Term]
  def args: Vector[ArgTermC[Term]]
  def implicitly: Boolean
  def cons: TelescopeTermF[Term, ThisTree]

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
      args: Vector[ArgTermC[Term]] = args,
      implicitly: Boolean = implicitly,
      meta: OptionTermMeta = meta
  ): ThisTree = cons.newTelescope(args, implicitly, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(args = args.map(g))
  )
}

@FunctionalInterface
trait FunctionF[Term <: TermT[Term], ThisTree <: FunctionC[Term]] {
  def newFunction(ty: FunctionTypeC[Term], body: Term, meta: OptionTermMeta): ThisTree
}

trait FunctionC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: FunctionC[Term]

  def ty: FunctionTypeC[Term]
  def body: Term
  def cons: FunctionF[Term, ThisTree]

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

  def cpy(ty: FunctionTypeC[Term] = ty, body: Term = body, meta: OptionTermMeta = meta): ThisTree = cons.newFunction(ty, body, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(ty = g(ty), body = f(body)))
}

@FunctionalInterface
trait FunctionTypeF[Term <: TermT[Term], ThisTree <: FunctionTypeC[Term]] {
  def newFunctionType(
      telescope: Vector[TelescopeTermC[Term]],
      resultTy: Term,
      effects: EffectsM,
      meta: OptionTermMeta
  ): ThisTree
}

trait FunctionTypeC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: FunctionTypeC[Term]

  def telescope: Vector[TelescopeTermC[Term]]
  def resultTy: Term
  def effects: EffectsM
  def cons: FunctionTypeF[Term, ThisTree]

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
      telescope: Vector[TelescopeTermC[Term]] = telescope,
      resultTy: Term = resultTy,
      effects: EffectsM = effects,
      meta: OptionTermMeta = meta
  ): ThisTree = cons.newFunctionType(telescope, resultTy, effects, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(
      telescope = telescope.map(g),
      resultTy = f(resultTy),
      effects = g(effects)
    )
  )
}

@FunctionalInterface
trait ObjectClauseValueTermF[Term <: TermT[Term], ThisTree <: ObjectClauseValueTermC[Term]] {
  def newObjectClauseValueTerm(key: Term, value: Term, meta: OptionTermMeta): ThisTree
}

trait ObjectClauseValueTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: ObjectClauseValueTermC[Term]
  def key: Term
  def value: Term
  def cons: ObjectClauseValueTermF[Term, ThisTree]

  override def toTerm: ObjectClauseValueTerm = ObjectClauseValueTerm(key.toTerm, value.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = group(
    key.toDoc <+> Doc.text("=") <+> value.toDoc
  )

  def cpy(key: Term = key, value: Term = value, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectClauseValueTerm(key, value, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(key = f(key), value = f(value))
  )
}

@FunctionalInterface
trait ObjectTermF[Term <: TermT[Term], ThisTree <: ObjectTermC[Term]] {
  def newObjectTerm(clauses: Vector[ObjectClauseValueTermC[Term]], meta: OptionTermMeta): ThisTree
}

trait ObjectTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: ObjectTermC[Term]
  def clauses: Vector[ObjectClauseValueTermC[Term]]
  def cons: ObjectTermF[Term, ThisTree]

  override def toTerm: ObjectTerm = ObjectTerm(clauses.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc))

  def cpy(clauses: Vector[ObjectClauseValueTermC[Term]] = clauses, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectTerm(clauses, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(clauses = clauses.map(g))
  )
}

// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
@FunctionalInterface
trait ObjectTypeF[Term <: TermT[Term], ThisTree <: ObjectTypeC[Term]] {
  def newObjectType(fieldTypes: Vector[ObjectClauseValueTermC[Term]], exactFields: Boolean, meta: OptionTermMeta): ThisTree
}

trait ObjectTypeC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: ObjectTypeC[Term]
  def fieldTypes: Vector[ObjectClauseValueTermC[Term]]
  def exactFields: Boolean
  def cons: ObjectTypeF[Term, ThisTree]

  override def toTerm: ObjectType = ObjectType(fieldTypes.map(_.toTerm), exactFields, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(
      fieldTypes.map(_.toDoc)
    )

  def cpy(fieldTypes: Vector[ObjectClauseValueTermC[Term]] = fieldTypes, exactFields: Boolean = exactFields, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectType(fieldTypes, exactFields, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(fieldTypes = fieldTypes.map(g))
  )
}

trait ReferenceCallC[Term <: TermT[Term]] extends UnevalT[Term] with TermWithUniqidT[Term] {
  override type ThisTree <: ReferenceCallC[Term]
  @deprecated("dont use")
  def name: Name
  def ty: Term

  override def descent(f: Term => Term, g: TreeMap[Term]): Term
}

@FunctionalInterface
trait LocalVF[Term <: TermT[Term], ThisTree <: LocalVC[Term]] {
  def newLocalV(name: Name, ty: Term, uniqId: UniqidOf[LocalVC[Term]], meta: OptionTermMeta): ThisTree
}

trait LocalVC[Term <: TermT[Term]] extends ReferenceCallC[Term] {
  override type ThisTree <: LocalVC[Term]

  @deprecated("dont use")
  def name: Name
  def ty: Term
  def uniqId: UniqidOf[LocalVC[Term]]
  def cons: LocalVF[Term, ThisTree]

  override def toTerm: LocalV = LocalV(name, ty.toTerm, uniqId, meta)

  override def toDoc(using options: PrettierOptions): Doc = Doc.text(name)

  def cpy(name: Name = name, ty: Term = ty, uniqId: UniqidOf[LocalVC[Term]] = uniqId, meta: OptionTermMeta = meta): ThisTree =
    cons.newLocalV(name, ty, uniqId, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(ty = f(ty)))
}

@FunctionalInterface
trait ToplevelVF[Term <: TermT[Term], ThisTree <: ToplevelVC[Term]] {
  def newToplevelV(id: AbsoluteRef, ty: Term, uniqId: UniqidOf[ToplevelVC[Term]], meta: OptionTermMeta): ThisTree
}

trait ToplevelVC[Term <: TermT[Term]] extends ReferenceCallC[Term] {
  override type ThisTree <: ToplevelVC[Term]

  def id: AbsoluteRef
  def ty: Term
  def uniqId: UniqidOf[ToplevelVC[Term]]
  def cons: ToplevelVF[Term, ThisTree]

  override def toTerm: ToplevelV = ToplevelV(id, ty.toTerm, uniqId, meta)

  override def toDoc(using options: PrettierOptions): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )

  def cpy(id: AbsoluteRef = id, ty: Term = ty, uniqId: UniqidOf[ToplevelVC[Term]] = uniqId, meta: OptionTermMeta = meta): ThisTree =
    cons.newToplevelV(id, ty, uniqId, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(ty = f(ty)))

  @deprecated("dont use")
  def name = id.name
}

trait ErrorTermC[Term <: TermT[Term]] extends SpecialTermT[Term] {
  override type ThisTree <: ErrorTermC[Term]
  def problem: Problem
  override def toTerm: ErrorTerm = ErrorTerm(problem, meta)

  override def toDoc(using options: PrettierOptions): Doc = problem.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}

trait StmtTermT[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: StmtTermT[Term]
}

@FunctionalInterface
trait LetStmtTermF[Term <: TermT[Term], ThisTree <: LetStmtTermC[Term]] {
  def newLetStmt(localv: LocalVC[Term], value: Term, ty: Term, meta: OptionTermMeta): ThisTree
}

trait LetStmtTermC[Term <: TermT[Term]] extends StmtTermT[Term] {
  override type ThisTree <: LetStmtTermC[Term]
  def localv: LocalVC[Term]
  def value: Term
  def ty: Term
  def cons: LetStmtTermF[Term, ThisTree]
  override def toTerm: LetStmtTerm = LetStmtTerm(localv.toTerm, value.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }

  def cpy(localv: LocalVC[Term] = localv, value: Term = value, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.newLetStmt(localv, value, ty, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}

@FunctionalInterface
trait DefStmtTermF[Term <: TermT[Term], ThisTree <: DefStmtTermC[Term]] {
  def newDefStmt(localv: LocalVC[Term], value: Term, ty: Term, meta: OptionTermMeta): ThisTree
}

trait DefStmtTermC[Term <: TermT[Term]] extends StmtTermT[Term] {
  override type ThisTree <: DefStmtTermC[Term]
  def localv: LocalVC[Term]
  def value: Term
  def ty: Term
  def cons: DefStmtTermF[Term, ThisTree]
  override def toTerm: DefStmtTerm = DefStmtTerm(localv.toTerm, value.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  }

  def cpy(localv: LocalVC[Term] = localv, value: Term = value, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.newDefStmt(localv, value, ty, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}

@FunctionalInterface
trait ExprStmtTermF[Term <: TermT[Term], ThisTree <: ExprStmtTermC[Term]] {
  def newExprStmt(expr: Term, ty: Term, meta: OptionTermMeta): ThisTree
}

trait ExprStmtTermC[Term <: TermT[Term]] extends StmtTermT[Term] {
  override type ThisTree <: ExprStmtTermC[Term]
  def expr: Term
  def ty: Term
  def cons: ExprStmtTermF[Term, ThisTree]

  override def toTerm: ExprStmtTerm = ExprStmtTerm(expr.toTerm, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = expr.toDoc

  def cpy(expr: Term = expr, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.newExprStmt(expr, ty, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(expr = f(expr), ty = f(ty))
  )
}

@FunctionalInterface
trait NonlocalOrLocalReturnF[Term <: TermT[Term], ThisTree <: NonlocalOrLocalReturnC[Term]] {
  def newNonlocalOrLocalReturn(value: Term, meta: OptionTermMeta): ThisTree
}

trait NonlocalOrLocalReturnC[Term <: TermT[Term]] extends StmtTermT[Term] {
  override type ThisTree <: NonlocalOrLocalReturnC[Term]
  def value: Term
  def cons: NonlocalOrLocalReturnF[Term, ThisTree]

  override def toTerm: NonlocalOrLocalReturn = NonlocalOrLocalReturn(value.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("return") <+> value.toDoc

  def cpy(value: Term = value, meta: OptionTermMeta = meta): ThisTree =
    cons.newNonlocalOrLocalReturn(value, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(value = f(value))
  )
}

trait TupleTypeC[Term <: TermT[Term]] extends TypeTermT[Term] {
  override type ThisTree <: TupleTypeC[Term]
  def types: Vector[Term]
  override def toTerm: TupleType = TupleType(types.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`, ",")(types)
  }

  def cpy(types: Vector[Term] = types, meta: OptionTermMeta = meta): ThisTree =
    cons.apply(types, meta)

  def cons: TupleTypeF[Term, ThisTree]

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(types = types.map(f))
  )
}

@FunctionalInterface
trait TupleTypeF[Term <: TermT[Term], ThisTree <: TupleTypeC[Term]] {
  def apply(types: Vector[Term], meta: OptionTermMeta): ThisTree
}

trait TupleTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: TupleTermC[Term]
  def values: Vector[Term]
  override def toTerm: TupleTerm = TupleTerm(values.map(_.toTerm), meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`(`, Docs.`)`, ",")(values)
  }

  def cpy(values: Vector[Term] = values, meta: OptionTermMeta = meta): ThisTree =
    cons.apply(values, meta)

  def cons: TupleTermF[Term, ThisTree]

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(values = values.map(f))
  )
}

@FunctionalInterface
trait TupleTermF[Term <: TermT[Term], ThisTree <: TupleTermC[Term]] {
  def apply(values: Vector[Term], meta: OptionTermMeta): ThisTree
}

@FunctionalInterface
trait BlockTermF[Term <: TermT[Term], ThisTree <: BlockTermC[Term]] {
  def newBlockTerm(statements: Vector[StmtTermT[Term]], result: Term, meta: OptionTermMeta): ThisTree
}

trait BlockTermC[Term <: TermT[Term]] extends UnevalT[Term] {
  override type ThisTree <: BlockTermC[Term]
  def statements: Vector[StmtTermT[Term]]
  def result: Term
  def cons: BlockTermF[Term, ThisTree]

  override def toTerm: BlockTerm = BlockTerm(statements.map(_.toTerm), result.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ";")(
      (statements.map(_.toDoc) :+ result.toDoc)
    )
  }

  def cpy(statements: Vector[StmtTermT[Term]] = statements, result: Term = result, meta: OptionTermMeta = meta): ThisTree =
    cons.newBlockTerm(statements, result, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(
      statements = statements.map(g),
      result = f(result)
    )
  )
}

@FunctionalInterface
trait AnnotationF[Term <: TermT[Term], ThisTree <: AnnotationC[Term]] {
  def newAnnotation(term: Term, ty: Option[Term], effects: Option[EffectsM], meta: OptionTermMeta): ThisTree
}

trait AnnotationC[Term <: TermT[Term]] extends UnevalT[Term] {
  override type ThisTree <: AnnotationC[Term]
  def term: Term
  def ty: Option[Term]
  def effects: Option[EffectsM]
  def cons: AnnotationF[Term, ThisTree]

  override def toTerm: Annotation = Annotation(term.toTerm, ty.map(_.toTerm), effects, meta)

  override def toDoc(using options: PrettierOptions): Doc = {
    val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
    val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
    term.toDoc <> tyDoc <> effectsDoc
  }

  def cpy(term: Term = term, ty: Option[Term] = ty, effects: Option[EffectsM] = effects, meta: OptionTermMeta = meta): ThisTree =
    cons.newAnnotation(term, ty, effects, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(x => g(x).asInstanceOf[EffectsM])
    )
  )
}

trait FieldTermC[Term <: TermT[Term]] extends WHNFT[Term] {
  override type ThisTree <: FieldTermC[Term]
  def name: Name
  def ty: Term
  def cons: FieldTermF[Term, ThisTree]
  override def toTerm: FieldTerm = FieldTerm(name, ty.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(name) <> Doc.text(": ") <> ty.toDoc

  def cpy(name: Name = name, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
    cons.apply(name, ty, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(ty = f(ty))
  )
}

@FunctionalInterface
trait FieldTermF[Term <: TermT[Term], ThisTree <: FieldTermC[Term]] {
  def apply(name: Name, ty: Term, meta: OptionTermMeta): ThisTree
}

trait TypeDefinitionT[Term <: TermT[Term]] extends StmtTermT[Term] with TermWithUniqidT[Term] {
  @deprecated("dont use")
  def name: Name
  override type ThisTree <: TypeDefinitionT[Term]
}

@FunctionalInterface
trait RecordStmtTermF[Term <: TermT[Term], ThisTree <: RecordStmtTermC[Term]] {
  def newRecordStmt(
      name: Name,
      uniqId: UniqidOf[RecordStmtTermC[Term]],
      fields: Vector[FieldTermC[Term]],
      body: Option[BlockTermC[Term]],
      meta: OptionTermMeta
  ): ThisTree
}

trait RecordStmtTermC[Term <: TermT[Term]] extends TypeDefinitionT[Term] with StmtTermT[Term] {
  override type ThisTree <: RecordStmtTermC[Term]
  def name: Name
  def uniqId: UniqidOf[RecordStmtTermC[Term]]
  def fields: Vector[FieldTermC[Term]]
  def body: Option[BlockTermC[Term]]
  def cons: RecordStmtTermF[Term, ThisTree]
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
      uniqId: UniqidOf[RecordStmtTermC[Term]] = uniqId,
      fields: Vector[FieldTermC[Term]] = fields,
      body: Option[BlockTermC[Term]] = body,
      meta: OptionTermMeta = meta
  ): ThisTree =
    cons.newRecordStmt(name, uniqId, fields, body, meta)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(
      fields = fields.map(g),
      body = body.map(g)
    )
  )

}

@FunctionalInterface
trait ObjectCallTermF[Term <: TermT[Term], ThisTree <: ObjectCallTermC[Term]] {
  def newObjectCallTerm(objectRef: Term, meta: OptionTermMeta): ThisTree
}

trait ObjectCallTermC[Term <: TermT[Term]] extends UnevalT[Term] {
  override type ThisTree <: ObjectCallTermC[Term]
  def objectRef: Term
  def cons: ObjectCallTermF[Term, ThisTree]

  override def toTerm: ObjectCallTerm = ObjectCallTerm(objectRef.toTerm, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  def cpy(objectRef: Term = objectRef, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectCallTerm(objectRef, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(objectRef = f(objectRef))
  )
}

@FunctionalInterface
trait ObjectTypeTermF[Term <: TermT[Term], ThisTree <: ObjectTypeTermC[Term]] {
  def newObjectTypeTerm(objectDef: ObjectStmtTerm, meta: OptionTermMeta): ThisTree
}

trait ObjectTypeTermC[Term <: TermT[Term]] extends TypeTermT[Term] {
  override type ThisTree <: ObjectTypeTermC[Term]
  def objectDef: ObjectStmtTerm
  def cons: ObjectTypeTermF[Term, ThisTree]

  override def toTerm: ObjectTypeTerm = ObjectTypeTerm(objectDef, meta)

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

  def cpy(objectDef: ObjectStmtTerm = objectDef, meta: OptionTermMeta = meta): ThisTree =
    cons.newObjectTypeTerm(objectDef, meta)

  def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    cpy(objectDef = g(objectDef))
  )
}
