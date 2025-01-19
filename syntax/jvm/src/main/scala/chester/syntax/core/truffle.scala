package chester.syntax.core
import chester.error.*
import upickle.default.*
import cats.data.*
import chester.syntax.*
import chester.syntax.core.spec.spec.*
import chester.uniqid.*
import chester.utils.*
import com.oracle.truffle.api.frame.VirtualFrame
import spire.math.*

import scala.collection.immutable.{ArraySeq, HashMap}
object truffle {
  type ExecuteGeneric = (VirtualFrame, Term) => Object
  val globalExecuteGeneric: Parameter[ExecuteGeneric] = new Parameter[ExecuteGeneric]
  given rw: ReadWriter[Term] = implicitly[ReadWriter[simple.Term]].bimap(convertToSimple, convertToTruffle)
  given rw1: ReadWriter[ReferenceCall] = rw.asInstanceOf[ReadWriter[ReferenceCall]]
  given rw2: ReadWriter[Effects] = rw.asInstanceOf[ReadWriter[Effects]]
  given rw3: ReadWriter[BlockTerm] = rw.asInstanceOf[ReadWriter[BlockTerm]]
  sealed abstract class Term extends com.oracle.truffle.api.nodes.Node with TermT[Term] {
    type ThisTree <: Term
    final def executeGeneric(frame: VirtualFrame): Object = globalExecuteGeneric.get(frame, this)
  }
  case class CallingArgTerm(
      @child var value: Term,
      @child var ty: Term,
      @const name: Option[Name] = None,
      @const vararg: Boolean = false,
      @const meta: OptionTermMeta
  ) extends WHNF
      with CallingArgTermC[Term] {
    override def cons: CallingArgTermF[Term, ThisTree] = this.copy
    override type ThisTree = CallingArgTerm
  }
  class Calling(
      args_ : Seq[CallingArgTerm],
      @const val implicitly: Boolean = false,
      @const val meta: OptionTermMeta
  ) extends WHNF
      with CallingC[Term] {
    @children private val argsArray = args_.toArray
    override def args: Seq[CallingArgTerm] = ArraySeq.unsafeWrapArray(argsArray)
    override type ThisTree = Calling
    override def cons: CallingF[Term, ThisTree] = Calling
  }
  implicit object Calling extends CallingF[Term, Calling] {
    def apply(args: Seq[CallingArgTerm], implicitly: Boolean = false, meta: OptionTermMeta = None): Calling = new Calling(args, implicitly, meta)
    def newCalling(args: Seq[CallingArgTermC[Term]], implicitly: Boolean = false, meta: OptionTermMeta = None): Calling =
      new Calling(args, implicitly, meta)
  }
  case class FCallTerm(
      @child var f: Term,
      @const args: Vector[Calling],
      @const meta: OptionTermMeta
  ) extends WHNF
      with FCallTermC[Term] {
    override type ThisTree = FCallTerm
    override def cons: FCallTermF[Term, ThisTree] = FCallTerm(_, _, _)
  }
  sealed abstract class Pat extends SpecialTerm with PatT[Term] {
    override type ThisTree <: Pat
  }
  case class Bind(
      @child var bind: LocalV,
      @child var ty: Term,
      @const meta: OptionTermMeta
  ) extends Pat
      with BindC[Term] {
    override type ThisTree = Bind
    override def cons: BindF[Term, ThisTree] = Bind(_, _, _)
  }
  sealed trait WHNF extends Term with WHNFT[Term] {
    override type ThisTree <: WHNF
  }
  sealed abstract class Uneval extends Term with UnevalT[Term] {
    override type ThisTree <: Uneval
  }
  sealed trait SpecialTerm extends Term with SpecialTermT[Term] {
    override type ThisTree <: SpecialTerm
  }
  sealed trait TermWithUniqid extends Term with TermWithUniqidT[Term] {
    override type ThisTree <: TermWithUniqid
  }
  sealed abstract class EffectsM extends Term with EffectsMT[Term] {
    override type ThisTree <: EffectsM
  }
  case class MetaTerm(@const impl: HoldNotReadable[?], @const meta: OptionTermMeta) extends EffectsM with MetaTermC[Term] with SpecialTerm {
    override type ThisTree = MetaTerm
  }
  case class ListTerm(@const terms: Vector[Term], @const meta: OptionTermMeta) extends WHNF with ListTermC[Term] {
    override final type ThisTree = ListTerm
    override def cons: ListTermF[Term, ThisTree] = this.copy
  }
  sealed trait TypeTerm extends WHNF with TypeTermT[Term] {
    override type ThisTree <: TypeTerm
  }
  sealed abstract class Sort extends TypeTerm with SortT[Term] {
    override type ThisTree <: Sort
    def level: Term
  }
  given aTypeF: TypeF[Term, Type] = Type(Level0, meta = None).cons
  case class Type(@child var level: Term, @const meta: OptionTermMeta) extends Sort with TypeC[Term] {
    override type ThisTree = Type
    override def cons: TypeF[Term, ThisTree] = this.copy
  }
  case class LevelType(@const meta: OptionTermMeta) extends TypeTerm with LevelTypeC[Term] {
    override type ThisTree = LevelType
    override def cons: LevelTypeF[Term, ThisTree] = this.copy
  }
  sealed abstract class Level extends WHNF with LevelT[Term] {
    type ThisTree <: Level
  }
  given aLevelFiniteF: LevelFiniteF[Term, LevelFinite] = LevelFinite(IntegerTerm(0, meta = None), meta = None).cons
  case class LevelFinite(@child var n: Term, @const meta: OptionTermMeta) extends Level with LevelFiniteC[Term] {
    override type ThisTree = LevelFinite
    override def cons: LevelFiniteF[Term, ThisTree] = this.copy
  }
  case class LevelUnrestricted(@const meta: OptionTermMeta) extends Level with LevelUnrestrictedC[Term] {
    override type ThisTree = LevelUnrestricted
    override def cons: LevelUnrestrictedF[Term, ThisTree] = this.copy
  }
  case class Prop(@child var level: Term, @const meta: OptionTermMeta) extends Sort with PropC[Term] {
    override type ThisTree = Prop
    override def cons: PropF[Term, ThisTree] = this.copy
  }
  // fibrant types
  case class FType(@child var level: Term, @const meta: OptionTermMeta) extends Sort with FTypeC[Term] {
    override type ThisTree = FType
    override def cons: FTypeF[Term, ThisTree] = this.copy
  }
  sealed abstract class LiteralTerm extends WHNF with LiteralTermT[Term] {
    override type ThisTree <: LiteralTerm
  }
  sealed abstract class AbstractIntTerm extends LiteralTerm with AbstractIntTermT[Term] {
    override type ThisTree <: AbstractIntTerm
  }
  case class IntTerm(@const value: Int, @const meta: OptionTermMeta) extends AbstractIntTerm with IntTermC[Term] {
    override type ThisTree = IntTerm
    override def cons: IntTermF[Term, ThisTree] = this.copy
  }
  given aIntegerTerm: IntegerTermF[Term, IntegerTerm] = IntegerTerm(0, meta = None).cons
  case class IntegerTerm(@const value: BigInt, @const meta: OptionTermMeta) extends AbstractIntTerm with IntegerTermC[Term] {
    override type ThisTree = IntegerTerm
    override def cons: IntegerTermF[Term, ThisTree] = this.copy
  }
  case class IntegerType(@const meta: OptionTermMeta) extends TypeTerm with IntegerTypeC[Term] {
    override type ThisTree = IntegerType
    override def cons: IntegerTypeF[Term, ThisTree] = this.copy
  }
  // int of 64 bits or more
  case class IntType(@const meta: OptionTermMeta) extends TypeTerm with IntTypeC[Term] {
    override type ThisTree = IntType
    override def cons: IntTypeF[Term, ThisTree] = this.copy
  }
  // unsigned int of 64 bits or more
  case class UIntType(@const meta: OptionTermMeta) extends TypeTerm with UIntTypeC[Term] {
    override type ThisTree = UIntType
    override def cons: UIntTypeF[Term, ThisTree] = this.copy
  }
  case class NaturalType(@const meta: OptionTermMeta) extends TypeTerm with NaturalTypeC[Term] {
    override type ThisTree = NaturalType
    override def cons: NaturalTypeF[Term, ThisTree] = this.copy
  }
  case class RationalTerm(@const value: Rational, @const meta: OptionTermMeta) extends LiteralTerm with RationalTermC[Term] {
    override type ThisTree = RationalTerm
    override def cons: RationalTermF[Term, ThisTree] = this.copy
  }
  case class BooleanTerm(@const value: Boolean, @const meta: OptionTermMeta) extends LiteralTerm with BooleanTermC[Term] {
    override type ThisTree = BooleanTerm
    override def cons: BooleanTermF[Term, ThisTree] = this.copy
  }
  case class BooleanType(@const meta: OptionTermMeta) extends TypeTerm with BooleanTypeC[Term] {
    override type ThisTree = BooleanType
    override def cons: BooleanTypeF[Term, ThisTree] = this.copy
  }
  case class StringTerm(@const value: String, @const meta: OptionTermMeta) extends LiteralTerm with StringTermC[Term] {
    override type ThisTree = StringTerm
    override def cons: StringTermF[Term, ThisTree] = this.copy
  }
  case class SymbolTerm(@const value: String, @const meta: OptionTermMeta) extends LiteralTerm with SymbolTermC[Term] {
    override type ThisTree = SymbolTerm
    override def cons: SymbolTermF[Term, ThisTree] = this.copy
  }
  case class RationalType(@const meta: OptionTermMeta) extends TypeTerm with RationalTypeC[Term] {
    override type ThisTree = RationalType
    override def cons: RationalTypeF[Term, ThisTree] = this.copy
  }
  // float of 32 bits or more
  case class FloatType(@const meta: OptionTermMeta) extends TypeTerm with FloatTypeC[Term] {
    override type ThisTree = FloatType
    override def cons: FloatTypeF[Term, ThisTree] = this.copy
  }
  case class StringType(@const meta: OptionTermMeta) extends TypeTerm with StringTypeC[Term] {
    override type ThisTree = StringType
    override def cons: StringTypeF[Term, ThisTree] = this.copy
  }
  case class SymbolType(@const meta: OptionTermMeta) extends TypeTerm with SymbolTypeC[Term] {
    override type ThisTree = SymbolType
    override def cons: SymbolTypeF[Term, ThisTree] = this.copy
  }
  case class AnyType(@child var level: Term, @const meta: OptionTermMeta) extends TypeTerm with AnyTypeC[Term] {
    override type ThisTree = AnyType
    override def cons: AnyTypeF[Term, ThisTree] = this.copy
  }
  given aAnyTypeF: AnyTypeF[Term, AnyType] = AnyType(Level0, meta = None).cons
  case class NothingType(@const meta: OptionTermMeta) extends TypeTerm with NothingTypeC[Term] {
    override type ThisTree = NothingType
    override def cons: NothingTypeF[Term, ThisTree] = this.copy
  }
  case class LiteralType(
      @child var literal: LiteralTerm,
      @const meta: OptionTermMeta
  ) extends TypeTerm
      with LiteralTypeC[Term] {
    override type ThisTree = LiteralType
    override def cons: LiteralTypeF[Term, ThisTree] = LiteralType(_, _)
  }
  case class ArgTerm(
      @child var bind: LocalV,
      @child var ty: Term,
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
    override def cons: TelescopeTermF[Term, ThisTree] = TelescopeTerm(_, _, _)
  }
  case class Function(
      @child var ty: FunctionType,
      @child var body: Term,
      @const meta: OptionTermMeta
  ) extends WHNF
      with FunctionC[Term] {
    override type ThisTree = Function
    override def cons: FunctionF[Term, ThisTree] = Function(_, _, _)
  }
  case class FunctionType(
      @const telescope: Vector[TelescopeTerm],
      @child var resultTy: Term,
      @const effects: EffectsM = Effects.Empty,
      @const meta: OptionTermMeta
  ) extends WHNF
      with FunctionTypeC[Term] {
    override type ThisTree = FunctionType
    override def cons: FunctionTypeF[Term, ThisTree] = this.copy
  }
  case class ObjectClauseValueTerm(
      @child var key: Term,
      @child var value: Term,
      @const meta: OptionTermMeta
  ) extends WHNF
      with ObjectClauseValueTermC[Term] {
    override type ThisTree = ObjectClauseValueTerm
    override def cons: ObjectClauseValueTermF[Term, ThisTree] = this.copy
  }
  case class ObjectTerm(
      @const clauses: Vector[ObjectClauseValueTerm],
      @const meta: OptionTermMeta
  ) extends WHNF
      with ObjectTermC[Term] {
    override type ThisTree = ObjectTerm
    override def cons: ObjectTermF[Term, ThisTree] = ObjectTerm(_, _)
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
  sealed abstract class Constructed extends WHNF with ConstructedT[Term] {
    type ThisTree <: Constructed
  }
  case class ListType(@child var ty: Term, @const meta: OptionTermMeta) extends Constructed with ListTypeC[Term] with TypeTerm {
    override type ThisTree = ListType
    override def cons: ListTypeF[Term, ThisTree] = this.copy
  }
  case class Union(@const xs: NonEmptyVector[Term], @const meta: OptionTermMeta) extends TypeTerm with UnionC[Term] {
    override type ThisTree = Union
    override def cons: UnionF[Term, ThisTree] = this.copy
  }
  case class Intersection(@const xs: NonEmptyVector[Term], @const meta: OptionTermMeta) extends TypeTerm with IntersectionC[Term] {
    override type ThisTree = Intersection
    override def cons: IntersectionF[Term, ThisTree] = this.copy
  }
  sealed abstract class Builtin extends WHNF with BuiltinT[Term] {
    override type ThisTree <: Builtin
  }
  sealed abstract class Effect extends WHNF with EffectT[Term] {
    override type ThisTree <: Effect
  }
  case class Effects(@const effectss: Map[LocalV, Term] = HashMap.empty, @const meta: OptionTermMeta) extends EffectsM with WHNF with EffectsC[Term] {
    override def effects = effectss
    override type ThisTree = Effects
    override def cons: EffectsF[Term, ThisTree] = this.copy
  }
  object Effects {
    val Empty: Effects = Effects(Map.empty, meta = None)
  }
  case class ExceptionEffect(@const meta: OptionTermMeta) extends Effect with ExceptionEffectC[Term] {
    override type ThisTree = ExceptionEffect
    override def cons: ExceptionEffectF[Term, ThisTree] = this.copy
  }
  sealed abstract class ReferenceCall extends Uneval with TermWithUniqid with ReferenceCallC[Term] {
    override type ThisTree <: ReferenceCall
  }
  case class LocalV(
      @const name: Name,
      @child var ty: Term,
      @const uniqId: UniqidOf[LocalV],
      @const meta: OptionTermMeta
  ) extends ReferenceCall
      with LocalVC[Term] {
    override type ThisTree = LocalV
    override def cons: LocalVF[Term, ThisTree] = this.copy
  }
  case class ToplevelV(
      @const id: AbsoluteRef,
      @child var ty: Term,
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
  sealed abstract class StmtTerm extends Term with StmtTermT[Term] {
    override type ThisTree <: StmtTerm
  }
  case class LetStmtTerm(
      @child var localv: LocalV,
      @child var value: Term,
      @child var ty: Term,
      @const meta: OptionTermMeta
  ) extends StmtTerm
      with LetStmtTermC[Term] {
    override type ThisTree = LetStmtTerm
    override def cons: LetStmtTermF[Term, ThisTree] = this.copy
  }
  case class DefStmtTerm(
      @child var localv: LocalV,
      @child var value: Term,
      @child var ty: Term,
      @const meta: OptionTermMeta
  ) extends StmtTerm
      with DefStmtTermC[Term] {
    override type ThisTree = DefStmtTerm
    override def cons: DefStmtTermF[Term, ThisTree] = this.copy
  }
  case class ExprStmtTerm(
      @child var expr: Term,
      @child var ty: Term = AnyType0,
      @const meta: OptionTermMeta
  ) extends StmtTerm
      with ExprStmtTermC[Term] {
    override type ThisTree = ExprStmtTerm
    override def cons: ExprStmtTermF[Term, ThisTree] = this.copy
  }
  case class NonlocalOrLocalReturn(@child var value: Term, @const meta: OptionTermMeta) extends StmtTerm with NonlocalOrLocalReturnC[Term] {
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
      @child var result: Term,
      @const meta: OptionTermMeta
  ) extends Uneval
      with BlockTermC[Term] {
    override type ThisTree = BlockTerm
    override def cons: BlockTermF[Term, ThisTree] = BlockTerm(_, _, _)
  }
  case class Annotation(
      @child var term: Term,
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
      @child var ty: Term,
      @const meta: OptionTermMeta
  ) extends WHNF
      with FieldTermC[Term] {
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
      with TraitStmtTermC[Term] {
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
      with InterfaceStmtTermC[Term] {
    override type ThisTree = InterfaceStmtTerm
    override def cons: InterfaceStmtTermF[Term, ThisTree] = this.copy
  }
  case class RecordCallTerm(
      recordDef: RecordStmtTerm,
      telescope: TelescopeTerm,
      meta: OptionTermMeta
  ) extends TypeTerm
      with RecordCallTermC[Term] {
    override type ThisTree = RecordCallTerm
    override def cons: RecordCallTermF[Term, ThisTree] = this.copy
  }
  case class ObjectCallTerm(
      @child var objectRef: Term,
      @const meta: OptionTermMeta
  ) extends Uneval
      with ObjectCallTermC[Term] {
    override type ThisTree = ObjectCallTerm
    override def cons: ObjectCallTermF[Term, ThisTree] = this.copy
  }
  case class ObjectTypeTerm(
      @child var objectDef: ObjectStmtTerm,
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
      with ObjectStmtTermC[Term] {
    override type ThisTree = ObjectStmtTerm
    override def cons: ObjectStmtTermF[Term, ThisTree] = this.copy
  }
  sealed abstract class TypeDefinition extends StmtTerm with TermWithUniqid with TypeDefinitionT[Term] {
    override type ThisTree <: TypeDefinition
  }
  case class FieldAccessTerm(
      @child var record: Term,
      @const fieldName: Name,
      @child var fieldType: Term,
      @const meta: OptionTermMeta
  ) extends Uneval
      with FieldAccessTermC[Term] {
    override type ThisTree = FieldAccessTerm
    override def cons: FieldAccessTermF[Term, ThisTree] = this.copy
  }
}
