// TODO: More correctly implement toDoc
package chester.syntax.core

import chester.doc.*
import chester.doc.const.{ColorProfile, Docs}
import chester.error.*
import chester.syntax.core.orm.*
import chester.syntax.*
import chester.utils.*
import chester.utils.doc.*
import spire.math.{Rational, Trilean}
import spire.math.Trilean.*
import upickle.default.*
import chester.uniqid.*

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

trait PatT[Rec <: TermT[Rec]] extends SpecialTermT[Rec] {
  override type ThisTree <: PatT[Rec]
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

/** more abstract Term. sealed trait *T corresponds to sealed trait in Term; trait *C corresponds to case class in Term */
trait TermT[Rec <: TermT[Rec]] extends Any with ToDoc with Tree[Rec]{
  override def toDoc(using options: PrettierOptions): Doc = toString

  def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec

  def mapFlatten[B](f: Rec => Seq[B]): Vector[B] = {
    var result = Vector.empty[B]
    inspectRecursive { term =>
      result ++= f(term)
    }
    result
  }

  def meta: OptionTermMeta
  def whnf: Trilean
  def toTerm: Term = {
    assert(this.isInstanceOf[Term], "forgot to implement toTerm?")
    this.asInstanceOf[Term]
  }
  def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)
}

type AnyTerm = TermT[?]

trait WHNFT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WHNFT[Rec]
  override def whnf: Trilean = True
}

trait UnevalT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: UnevalT[Rec]
  override def whnf: Trilean = False
}

trait SpecialTermT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: SpecialTermT[Rec]
  override def whnf: Trilean = Unknown
}

trait TermWithUniqidT[Rec <: TermT[Rec]] extends TermT[Rec] with HasUniqid {
  override type ThisTree <: TermWithUniqidT[Rec]
  override def uniqId: UniqidOf[TermT[Rec]]
}

trait EffectsMT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: EffectsMT[Rec]
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

trait ListTermC[Rec <: TermT[Rec]] extends TermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: ListTermC[Rec]
  def terms: Vector[Rec]
  override def toTerm: ListTerm = ListTerm(terms.map(_.toTerm), meta)
}

trait TypeTermT[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: TypeTermT[Rec]
}

trait SortT[Rec <: TermT[Rec]] extends TypeTermT[Rec] {
  override type ThisTree <: SortT[Rec]
  def level: TermT[Rec]
}

trait TypeT[Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: TypeT[Rec]
  def level: Rec
  override def toTerm: Type = Type(level.toTerm, meta)
}

trait LevelTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: LevelTypeC[Rec]
  override def toTerm: LevelType = LevelType(meta)
}

trait LevelT[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: LevelT[Rec]
}

trait LevelFiniteC[Rec <: TermT[Rec]] extends LevelT[Rec] {
  override type ThisTree <: LevelFiniteC[Rec]
  def n: Rec
  override def toTerm: LevelFinite = LevelFinite(n.toTerm, meta)
}

trait LevelUnrestrictedC[Rec <: TermT[Rec]] extends LevelT[Rec] {
  override type ThisTree <: LevelUnrestrictedC[Rec]
  override def toTerm: LevelUnrestricted = LevelUnrestricted(meta)
}

enum Usage derives ReadWriter {
  case None, Linear, Unrestricted
}

trait PropC[Rec <: TermT[Rec]] extends SortT[Rec] {
  override type ThisTree <: PropC[Rec]
  def level: Rec
  override def toTerm: Prop = Prop(level.toTerm, meta)
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

trait LiteralTermT[Rec <: TermT[Rec]] extends TermT[Rec] with WHNFT[Rec] {
  override type ThisTree <: LiteralTermT[Rec]
}

trait AbstractIntTermT[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: AbstractIntTermT[Rec]
}

trait IntTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] with AbstractIntTermT[Rec] {
  override type ThisTree <: IntTermC[Rec]
  def value: Int
  override def toTerm: IntTerm = IntTerm(value, meta)
}

trait IntegerTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] with AbstractIntTermT[Rec] {
  override type ThisTree <: IntegerTermC[Rec]
  def value: BigInt
  override def toTerm: IntegerTerm = IntegerTerm(value, meta)
}

trait WithTypeT[Rec <: TermT[Rec]] extends TermT[Rec] {
  override type ThisTree <: WithTypeT[Rec]
  def ty: Rec
}

trait IntegerTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: IntegerTypeC[Rec]
  override def toTerm: IntegerType = IntegerType(meta)
}

trait IntTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: IntTypeC[Rec]
  override def toTerm: IntType = IntType(meta)
}

trait UIntTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: UIntTypeC[Rec]
  override def toTerm: UIntType = UIntType(meta)
}

trait NaturalTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: NaturalTypeC[Rec]
  override def toTerm: NaturalType = NaturalType(meta)
}

trait RationalTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: RationalTermC[Rec]
  def value: Rational
  override def toTerm: RationalTerm = RationalTerm(value, meta)
}

trait BooleanTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: BooleanTermC[Rec]
  def value: Boolean
  override def toTerm: BooleanTerm = BooleanTerm(value, meta)

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = this

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}

trait BooleanTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: BooleanTypeC[Rec]
  override def toTerm: BooleanType = BooleanType(meta)
}

trait StringTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: StringTermC[Rec]
  def value: String
  override def toTerm: StringTerm = StringTerm(value, meta)
}

trait SymbolTermC[Rec <: TermT[Rec]] extends LiteralTermT[Rec] {
  override type ThisTree <: SymbolTermC[Rec]
  def value: String
  override def toTerm: SymbolTerm = SymbolTerm(value, meta)
}

trait RationalTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: RationalTypeC[Rec]
  override def toTerm: RationalType = RationalType(meta)
}

trait FloatTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: FloatTypeC[Rec]
  override def toTerm: FloatType = FloatType(meta)
}

trait StringTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: StringTypeC[Rec]
  override def toTerm: StringType = StringType(meta)
}

trait SymbolTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: SymbolTypeC[Rec]
  override def toTerm: SymbolType = SymbolType(meta)
}

trait AnyTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: AnyTypeC[Rec]
  def level: Rec
  override def toTerm: AnyType = AnyType(level.toTerm, meta)
}

trait NothingTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: NothingTypeC[Rec]
  override def toTerm: NothingType = NothingType(meta)
}

trait LiteralTypeC[Rec <: TermT[Rec]] extends TypeTermT[Rec] with WithTypeT[Rec] {
  override type ThisTree <: LiteralTypeC[Rec]
  def literal: LiteralTermT[Rec]
  override def toTerm: LiteralType = LiteralType(literal.toTerm.asInstanceOf[LiteralTerm], meta)
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

trait ReferenceCallC[Rec <: TermT[Rec]] extends UnevalT[Rec] with TermWithUniqidT[Rec] {
  override type ThisTree <: ReferenceCallC[Rec]
  @deprecated("dont use")
  def name: Name
  def ty: Rec

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec
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

trait ErrorTermC[Rec <: TermT[Rec]] extends SpecialTermT[Rec] {
  override type ThisTree <: ErrorTermC[Rec]
  def problem: Problem
  override def toTerm: ErrorTerm = ErrorTerm(problem, meta)

  override def toDoc(using options: PrettierOptions): Doc = problem.toDoc

  override def descent(f: Rec => Rec, g: TreeMap[Rec]): Rec = this
}

trait StmtTermT[Rec <: TermT[Rec]] extends WHNFT[Rec] {
  override type ThisTree <: StmtTermT[Rec]
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

trait TypeDefinitionT[Rec <: TermT[Rec]] extends StmtTermT[Rec] with TermWithUniqidT[Rec] {
  @deprecated("dont use")
  def name: Name
  override type ThisTree <: TypeDefinitionT[Rec]
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
