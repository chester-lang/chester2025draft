package chester.syntax.core
import chester.error.*
import chester.error.ProblemUpickle.*
import chester.utils.given
import chester.utils.impls.*
import upickle.default.*
import cats.data.*
import chester.error.Problem
import chester.syntax.*
import chester.syntax.core.spec.spec.*
import chester.uniqid.*
import chester.utils.*
import com.oracle.truffle.api.frame.VirtualFrame
import spire.math.*

import scala.collection.immutable.HashMap
object truffle {
  type ExecuteGeneric = (VirtualFrame, Term) => Object
  var globalExecuteGeneric: ExecuteGeneric = (_: VirtualFrame, _: Term) => ???
  sealed trait Term extends com.oracle.truffle.api.nodes.Node with TermT[Term] derives ReadWriter {
    type ThisTree <: Term
    final def executeGeneric(frame: VirtualFrame): Object = globalExecuteGeneric(frame, this)
  }
  case class CallingArgTerm(
      @child value: Term,
      @child ty: Term,
      @const name: Option[Name] = None,
      @const vararg: Boolean = false,
      @const meta: OptionTermMeta
  ) extends WHNF
      with CallingArgTermC[Term] derives ReadWriter {
    override def cons: CallingArgTermF[Term, ThisTree] = this.copy
    override type ThisTree = CallingArgTerm
  }
  case class Calling(
      @const args: Vector[CallingArgTerm],
      @const implicitly: Boolean = false,
      @const meta: OptionTermMeta
  ) extends WHNF
      with CallingC[Term] derives ReadWriter {
    override type ThisTree = Calling
    override def cons: CallingF[Term, ThisTree] = this.copy
  }
  case class FCallTerm(
      @child f: Term,
      @const args: Vector[Calling],
      @const meta: OptionTermMeta
  ) extends WHNF
      with FCallTermC[Term] {
    override type ThisTree = FCallTerm
    override def cons: FCallTermF[Term, ThisTree] = this.copy
  }
  sealed trait Pat extends SpecialTerm with PatT[Term] derives ReadWriter {
    override type ThisTree <: Pat
  }
  case class Bind(
      @child bind: LocalV,
      @child ty: Term,
      @const meta: OptionTermMeta
  ) extends Pat
      with BindC[Term] {
    override type ThisTree = Bind
    override def cons: BindF[Term, ThisTree] = this.copy
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
  }
  sealed trait EffectsM extends Term with EffectsMT[Term] derives ReadWriter {
    override type ThisTree <: EffectsM
  }
  case class MetaTerm(@const impl: HoldNotReadable[?], @const meta: OptionTermMeta) extends Term with MetaTermC[Term] with EffectsM with SpecialTerm {
    override type ThisTree = MetaTerm
  }
  case class ListTerm(@child terms: Vector[Term], @const meta: OptionTermMeta) extends Term with ListTermC[Term] with WHNF derives ReadWriter {
    override final type ThisTree = ListTerm
    override def cons: ListTermF[Term, ThisTree] = this.copy
  }
  sealed trait TypeTerm extends Term with TypeTermT[Term] with WHNF derives ReadWriter {
    override type ThisTree <: TypeTerm
  }
  sealed trait Sort extends TypeTerm with SortT[Term] derives ReadWriter {
    override type ThisTree <: Sort
    def level: Term
  }
  given aTypeF: TypeF[Term, Type] = Type(Level0, meta = None).cons
  case class Type(@child level: Term, @const meta: OptionTermMeta) extends Sort with TypeC[Term] {
    override type ThisTree = Type
    override def cons: TypeF[Term, ThisTree] = this.copy
  }
  case class LevelType(@const meta: OptionTermMeta) extends TypeTerm with LevelTypeC[Term] {
    override type ThisTree = LevelType
    override def cons: LevelTypeF[Term, ThisTree] = this.copy
  }
  sealed trait Level extends Term with LevelT[Term] with WHNF derives ReadWriter {
    type ThisTree <: Level
  }
  given aLevelFiniteF: LevelFiniteF[Term, LevelFinite] = LevelFinite(IntegerTerm(0, meta = None), meta = None).cons
  case class LevelFinite(@child n: Term, @const meta: OptionTermMeta) extends Level with LevelFiniteC[Term] {
    override type ThisTree = LevelFinite
    override def cons: LevelFiniteF[Term, ThisTree] = this.copy
  }
  case class LevelUnrestricted(@const meta: OptionTermMeta) extends Level with LevelUnrestrictedC[Term] {
    override type ThisTree = LevelUnrestricted
    override def cons: LevelUnrestrictedF[Term, ThisTree] = this.copy
  }
  case class Prop(@child level: Term, @const meta: OptionTermMeta) extends Sort with PropC[Term] {
    override type ThisTree = Prop
    override def cons: PropF[Term, ThisTree] = this.copy
  }
  // fibrant types
  case class FType(@child level: Term, @const meta: OptionTermMeta) extends Sort with FTypeC[Term] {
    override type ThisTree = FType
    override def cons: FTypeF[Term, ThisTree] = this.copy
  }
  sealed trait LiteralTerm extends Term with LiteralTermT[Term] with WHNF derives ReadWriter {
    override type ThisTree <: LiteralTerm
  }
  sealed trait AbstractIntTerm extends LiteralTerm with AbstractIntTermT[Term] derives ReadWriter {
    override type ThisTree <: AbstractIntTerm
  }
  case class IntTerm(@const value: Int, @const meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntTermC[Term] derives ReadWriter {
    override type ThisTree = IntTerm
    override def cons: IntTermF[Term, ThisTree] = this.copy
  }
  given aIntegerTerm: IntegerTermF[Term, IntegerTerm] = IntegerTerm(0, meta = None).cons
  case class IntegerTerm(@const value: BigInt, @const meta: OptionTermMeta) extends LiteralTerm with AbstractIntTerm with IntegerTermC[Term]
      derives ReadWriter {
    override type ThisTree = IntegerTerm
    override def cons: IntegerTermF[Term, ThisTree] = this.copy
  }
  case class IntegerType(@const meta: OptionTermMeta) extends TypeTerm with IntegerTypeC[Term] derives ReadWriter {
    override type ThisTree = IntegerType
    override def cons: IntegerTypeF[Term, ThisTree] = this.copy
  }
  // int of 64 bits or more
  case class IntType(@const meta: OptionTermMeta) extends TypeTerm with IntTypeC[Term] derives ReadWriter {
    override type ThisTree = IntType
    override def cons: IntTypeF[Term, ThisTree] = this.copy
  }
  // unsigned int of 64 bits or more
  case class UIntType(@const meta: OptionTermMeta) extends TypeTerm with UIntTypeC[Term] derives ReadWriter {
    override type ThisTree = UIntType
    override def cons: UIntTypeF[Term, ThisTree] = this.copy
  }
  case class NaturalType(@const meta: OptionTermMeta) extends TypeTerm with NaturalTypeC[Term] derives ReadWriter {
    override type ThisTree = NaturalType
    override def cons: NaturalTypeF[Term, ThisTree] = this.copy
  }
  case class RationalTerm(@const value: Rational, @const meta: OptionTermMeta) extends LiteralTerm with RationalTermC[Term] derives ReadWriter {
    override type ThisTree = RationalTerm
    override def cons: RationalTermF[Term, ThisTree] = this.copy
  }
  case class BooleanTerm(@const value: Boolean, @const meta: OptionTermMeta) extends LiteralTerm with BooleanTermC[Term] derives ReadWriter {
    override type ThisTree = BooleanTerm
    override def cons: BooleanTermF[Term, ThisTree] = this.copy
  }
  case class BooleanType(@const meta: OptionTermMeta) extends TypeTerm with BooleanTypeC[Term] derives ReadWriter {
    override type ThisTree = BooleanType
    override def cons: BooleanTypeF[Term, ThisTree] = this.copy
  }
  case class StringTerm(@const value: String, @const meta: OptionTermMeta) extends LiteralTerm with StringTermC[Term] derives ReadWriter {
    override type ThisTree = StringTerm
    override def cons: StringTermF[Term, ThisTree] = this.copy
  }
  case class SymbolTerm(@const value: String, @const meta: OptionTermMeta) extends LiteralTerm with SymbolTermC[Term] derives ReadWriter {
    override type ThisTree = SymbolTerm
    override def cons: SymbolTermF[Term, ThisTree] = this.copy
  }
  case class RationalType(@const meta: OptionTermMeta) extends TypeTerm with RationalTypeC[Term] derives ReadWriter {
    override type ThisTree = RationalType
    override def cons: RationalTypeF[Term, ThisTree] = this.copy
  }
  // float of 32 bits or more
  case class FloatType(@const meta: OptionTermMeta) extends TypeTerm with FloatTypeC[Term] derives ReadWriter {
    override type ThisTree = FloatType
    override def cons: FloatTypeF[Term, ThisTree] = this.copy
  }
  case class StringType(@const meta: OptionTermMeta) extends TypeTerm with StringTypeC[Term] derives ReadWriter {
    override type ThisTree = StringType
    override def cons: StringTypeF[Term, ThisTree] = this.copy
  }
  case class SymbolType(@const meta: OptionTermMeta) extends TypeTerm with SymbolTypeC[Term] derives ReadWriter {
    override type ThisTree = SymbolType
    override def cons: SymbolTypeF[Term, ThisTree] = this.copy
  }
  case class AnyType(@child level: Term, @const meta: OptionTermMeta) extends TypeTerm with AnyTypeC[Term] derives ReadWriter {
    override type ThisTree = AnyType
    override def cons: AnyTypeF[Term, ThisTree] = this.copy
  }
  given aAnyTypeF: AnyTypeF[Term, AnyType] = AnyType(Level0, meta = None).cons
  case class NothingType(@const meta: OptionTermMeta) extends TypeTerm with NothingTypeC[Term] {
    override type ThisTree = NothingType
    override def cons: NothingTypeF[Term, ThisTree] = this.copy
  }
  case class LiteralType(
      @child literal: LiteralTerm,
      @const meta: OptionTermMeta
  ) extends TypeTerm
      with LiteralTypeC[Term] {
    override type ThisTree = LiteralType
    override def cons: LiteralTypeF[Term, ThisTree] = this.copy
  }
  case class ArgTerm(
      @child bind: LocalV,
      @child ty: Term,
      @const default: Option[Term] = None,
      @const vararg: Boolean = false,
      @const meta: OptionTermMeta
  ) extends WHNF
      with ArgTermC[Term] {
    override type ThisTree = ArgTerm
    override def cons: ArgTermF[Term, ThisTree] = this.copy
  }
  case class TelescopeTerm(
      @const args: Vector[ArgTerm],
      @const implicitly: Boolean = false,
      @const meta: OptionTermMeta
  ) extends WHNF
      with TelescopeTermC[Term] {
    override type ThisTree = TelescopeTerm
    override def cons: TelescopeTermF[Term, ThisTree] = this.copy
  }
  case class Function(
      @child ty: FunctionType,
      @child body: Term,
      @const meta: OptionTermMeta
  ) extends WHNF
      with FunctionC[Term] {
    override type ThisTree = Function
    override def cons: FunctionF[Term, ThisTree] = this.copy
  }
  case class FunctionType(
      @const telescope: Vector[TelescopeTerm],
      @child resultTy: Term,
      @const effects: EffectsM = NoEffect,
      @const meta: OptionTermMeta
  ) extends WHNF
      with FunctionTypeC[Term] {
    override type ThisTree = FunctionType
    override def cons: FunctionTypeF[Term, ThisTree] = this.copy
  }
  case class ObjectClauseValueTerm(
      @child key: Term,
      @child value: Term,
      @const meta: OptionTermMeta
  ) extends WHNF
      with ObjectClauseValueTermC[Term] derives ReadWriter {
    override type ThisTree = ObjectClauseValueTerm
    override def cons: ObjectClauseValueTermF[Term, ThisTree] = this.copy
  }
  case class ObjectTerm(
      @const clauses: Vector[ObjectClauseValueTerm],
      @const meta: OptionTermMeta
  ) extends WHNF
      with ObjectTermC[Term] {
    override type ThisTree = ObjectTerm
    override def cons: ObjectTermF[Term, ThisTree] = this.copy
  }
  case class ObjectType(
      @const fieldTypes: Vector[ObjectClauseValueTerm],
      @const exactFields: Boolean = false,
      @const meta: OptionTermMeta
  ) extends WHNF
      with ObjectTypeC[Term] {
    override type ThisTree = ObjectType
    override def cons: ObjectTypeF[Term, ThisTree] = this.copy
  }
  case class ListF(@const meta: OptionTermMeta) extends Builtin with ListFC[Term] {
    override type ThisTree = ListF
    override def cons: ListFF[Term, ThisTree] = this.copy
  }
  sealed trait Constructed extends WHNF with ConstructedT[Term] derives ReadWriter {
    type ThisTree <: Constructed
  }
  case class ListType(@child ty: Term, @const meta: OptionTermMeta) extends Constructed with ListTypeC[Term] with TypeTerm {
    override type ThisTree = ListType
    override def cons: ListTypeF[Term, ThisTree] = this.copy
  }
  case class Union(@const xs: NonEmptyVector[Term], @const meta: OptionTermMeta) extends TypeTerm with UnionC[Term] {
    override type ThisTree = Union
    override def cons: UnionF[Term, ThisTree] = this.copy
  }
  case class Intersection(@const xs: NonEmptyVector[Term], @const meta: OptionTermMeta) extends TypeTerm with IntersectionC[Term] derives ReadWriter {
    override type ThisTree = Intersection
    override def cons: IntersectionF[Term, ThisTree] = this.copy
  }
  sealed trait Builtin extends WHNF with BuiltinT[Term] derives ReadWriter {
    override type ThisTree <: Builtin
  }
  sealed trait Effect extends WHNF with EffectT[Term] derives ReadWriter {
    override type ThisTree <: Effect
  }
  case class Effects(@const effectss: Map[LocalV, Term] = HashMap.empty, @const meta: OptionTermMeta) extends WHNF with EffectsM with EffectsC[Term]
      derives ReadWriter {
    override def effects = effectss
    override type ThisTree = Effects
    override def cons: EffectsF[Term, ThisTree] = this.copy
  }
  case class ExceptionEffect(@const meta: OptionTermMeta) extends Effect with ExceptionEffectC[Term] {
    override type ThisTree = ExceptionEffect
    override def cons: ExceptionEffectF[Term, ThisTree] = this.copy
  }
  sealed trait ReferenceCall extends Term with Uneval with TermWithUniqid with ReferenceCallC[Term] derives ReadWriter {
    override type ThisTree <: ReferenceCall
  }
  case class LocalV(
      @const name: Name,
      @child ty: Term,
      @const uniqId: UniqidOf[LocalV],
      @const meta: OptionTermMeta
  ) extends ReferenceCall
      with LocalVC[Term] {
    override type ThisTree = LocalV
    override def cons: LocalVF[Term, ThisTree] = this.copy
  }
  case class ToplevelV(
      @const id: AbsoluteRef,
      @child ty: Term,
      @const uniqId: UniqidOf[ToplevelV],
      @const meta: OptionTermMeta
  ) extends ReferenceCall
      with ToplevelVC[Term] {
    override type ThisTree = ToplevelV
    override def cons: ToplevelVF[Term, ThisTree] = this.copy
  }
  case class ErrorTerm(@const problem: Problem, @const meta: OptionTermMeta) extends SpecialTerm with ErrorTermC[Term] {
    override type ThisTree = ErrorTerm
  }
  sealed trait StmtTerm extends Term with StmtTermT[Term] derives ReadWriter {
    override type ThisTree <: StmtTerm
  }
  case class LetStmtTerm(
      @child localv: LocalV,
      @child value: Term,
      @child ty: Term,
      @const meta: OptionTermMeta
  ) extends StmtTerm
      with LetStmtTermC[Term] {
    override type ThisTree = LetStmtTerm
    override def cons: LetStmtTermF[Term, ThisTree] = this.copy
  }
  case class DefStmtTerm(
      @child localv: LocalV,
      @child value: Term,
      @child ty: Term,
      @const meta: OptionTermMeta
  ) extends StmtTerm
      with DefStmtTermC[Term] {
    override type ThisTree = DefStmtTerm
    override def cons: DefStmtTermF[Term, ThisTree] = this.copy
  }
  case class ExprStmtTerm(
      @child expr: Term,
      @child ty: Term = AnyType0,
      @const meta: OptionTermMeta
  ) extends StmtTerm
      with ExprStmtTermC[Term] {
    override type ThisTree = ExprStmtTerm
    override def cons: ExprStmtTermF[Term, ThisTree] = this.copy
  }
  case class NonlocalOrLocalReturn(@child value: Term, @const meta: OptionTermMeta) extends StmtTerm with NonlocalOrLocalReturnC[Term] {
    override type ThisTree = NonlocalOrLocalReturn
    override def cons: NonlocalOrLocalReturnF[Term, ThisTree] = this.copy
  }
  case class TupleType(@const types: Vector[Term], @const meta: OptionTermMeta) extends TypeTerm with TupleTypeC[Term] {
    override type ThisTree = TupleType
    override def cons: TupleTypeF[Term, ThisTree] = this.copy
  }
  case class TupleTerm(@const values: Vector[Term], @const meta: OptionTermMeta) extends WHNF with TupleTermC[Term] {
    override type ThisTree = TupleTerm
    override def cons: TupleTermF[Term, ThisTree] = this.copy
  }
  case class BlockTerm(
      @const statements: Vector[StmtTerm],
      @child result: Term,
      @const meta: OptionTermMeta
  ) extends Uneval
      with BlockTermC[Term] derives ReadWriter {
    override type ThisTree = BlockTerm
    override def cons: BlockTermF[Term, ThisTree] = this.copy
  }
  case class Annotation(
      @child term: Term,
      @const ty: Option[Term],
      @const effects: Option[EffectsM],
      @const meta: OptionTermMeta
  ) extends Uneval
      with AnnotationC[Term] {
    override type ThisTree = Annotation
    override def cons: AnnotationF[Term, ThisTree] = this.copy
  }
  case class FieldTerm(
      @const name: Name,
      @child ty: Term,
      @const meta: OptionTermMeta
  ) extends WHNF
      with FieldTermC[Term] derives ReadWriter {
    override type ThisTree = FieldTerm
    override def cons: FieldTermF[Term, ThisTree] = this.copy
  }
  case class RecordStmtTerm(
      @const name: Name,
      @const uniqId: UniqidOf[RecordStmtTerm] = Uniqid.generate[RecordStmtTerm],
      @const fields: Vector[FieldTerm],
      @const body: Option[BlockTerm],
      @const meta: OptionTermMeta
  ) extends TypeDefinition
      with RecordStmtTermC[Term] {
    override type ThisTree = RecordStmtTerm
    override def cons: RecordStmtTermF[Term, ThisTree] = this.copy
  }
  case class RecordConstructorCallTerm(
      @const recordName: Name,
      @const args: Vector[Term],
      @const meta: OptionTermMeta
  ) extends Uneval
      with RecordConstructorCallTermC[Term] {
    override type ThisTree = RecordConstructorCallTerm
    override def cons: RecordConstructorCallTermF[Term, ThisTree] = this.copy
  }
  case class TraitStmtTerm(
      @const name: Name,
      @const uniqId: UniqidOf[TraitStmtTerm] = Uniqid.generate[TraitStmtTerm],
      @const extendsClause: Option[Term] = None,
      @const body: Option[BlockTerm] = None,
      @const meta: OptionTermMeta
  ) extends TypeDefinition
      with TraitStmtTermC[Term] derives ReadWriter {
    override type ThisTree = TraitStmtTerm
    override def cons: TraitStmtTermF[Term, ThisTree] = this.copy
  }
  case class InterfaceStmtTerm(
      @const name: Name,
      @const uniqId: UniqidOf[InterfaceStmtTerm] = Uniqid.generate[InterfaceStmtTerm],
      @const extendsClause: Option[Term] = None,
      @const body: Option[BlockTerm] = None,
      @const meta: OptionTermMeta
  ) extends TypeDefinition
      with InterfaceStmtTermC[Term] derives ReadWriter {
    override type ThisTree = InterfaceStmtTerm
    override def cons: InterfaceStmtTermF[Term, ThisTree] = this.copy
  }
  case class ObjectCallTerm(
      @child objectRef: Term,
      @const meta: OptionTermMeta
  ) extends Uneval
      with ObjectCallTermC[Term] {
    override type ThisTree = ObjectCallTerm
    override def cons: ObjectCallTermF[Term, ThisTree] = this.copy
  }
  case class ObjectTypeTerm(
      @child objectDef: ObjectStmtTerm,
      @const meta: OptionTermMeta
  ) extends TypeTerm
      with ObjectTypeTermC[Term] {
    override type ThisTree = ObjectTypeTerm
    override def cons: ObjectTypeTermF[Term, ThisTree] = this.copy
  }
  case class ObjectStmtTerm(
      @const name: Name,
      @const uniqId: UniqidOf[ObjectStmtTerm],
      @const extendsClause: Option[Term],
      @const body: Option[BlockTerm],
      @const meta: OptionTermMeta
  ) extends TypeDefinition
      with ObjectStmtTermC[Term] derives ReadWriter {
    override type ThisTree = ObjectStmtTerm
    override def cons: ObjectStmtTermF[Term, ThisTree] = this.copy
  }
  sealed trait TypeDefinition extends StmtTerm with TermWithUniqid with TypeDefinitionT[Term] derives ReadWriter {
    override type ThisTree <: TypeDefinition
  }
}
