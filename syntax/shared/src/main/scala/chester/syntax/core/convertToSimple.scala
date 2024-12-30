package chester.syntax.core

import cats.data.NonEmptyVector
import chester.syntax.core.spec.spec.*

import scala.language.implicitConversions

def convertToSimple[Term <: TermT[Term]](term: Term): simple.Term = {
  implicit  def convertLiteralTermT[From <: LiteralTermT[Term]](t: From): simple.LiteralTerm =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[simple.LiteralTerm]
  implicit  def innerConvert[From <: TermT[Term]](t: From): simple.Term = convertToSimple(t.asInstanceOf[Term]).asInstanceOf[simple.Term]
  implicit  def innerCOnvertLocalV[From <: LocalVC[Term]](t: From): simple.LocalV =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[simple.LocalV]

  implicit  def innerConvertFunctionType[From <: FunctionTypeC[Term]](t: From): simple.FunctionType =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[simple.FunctionType]
  implicit  def innerConvertEffects[From <: EffectsMT[Term]](t: From): simple.EffectsM =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[simple.EffectsM]
  implicit  def innerXsN[From <: TermT[Term]](x: NonEmptyVector[From]): NonEmptyVector[simple.Term] =
    x.asInstanceOf[NonEmptyVector[Term]].map(convertToSimple).asInstanceOf[NonEmptyVector[simple.Term]]
  implicit  def innerXs[From <: TermT[Term]](x: Vector[From]): Vector[simple.Term] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.Term]]
  implicit  def innerXs2ss[From <: CallingArgTermC[Term]](x: Seq[From]): Seq[simple.CallingArgTerm] =
    x.asInstanceOf[Seq[Term]].map(convertToSimple).asInstanceOf[Seq[simple.CallingArgTerm]]
  implicit  def innerXs2[From <: CallingArgTermC[Term]](x: Vector[From]): Vector[simple.CallingArgTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.CallingArgTerm]]
  implicit  def innerXsCalling[From <: CallingC[Term]](x: Seq[From]): Seq[simple.Calling] =
    x.asInstanceOf[Seq[Term]].map(convertToSimple).asInstanceOf[Seq[simple.Calling]]
  implicit  def innerXsCallingff[From <: CallingC[Term]](x: Vector[From]): Vector[simple.Calling] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.Calling]]
  implicit  def innerXSTelescope[From <: TelescopeTermC[Term]](x: Vector[From]): Vector[simple.TelescopeTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.TelescopeTerm]]
  implicit  def innerXsArgTerm[From <: ArgTermC[Term]](x: Vector[From]): Vector[simple.ArgTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.ArgTerm]]
  implicit  def innerXsObjectClauseValueTerm[From <: ObjectClauseValueTermC[Term]](x: Vector[From]): Vector[simple.ObjectClauseValueTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.ObjectClauseValueTerm]]
  implicit  def innerXsStmtTerm[From <: StmtTermT[Term]](x: Vector[From]): Vector[simple.StmtTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.StmtTerm]]
  implicit  def innerXsFieldTerm[From <: FieldTermC[Term]](x: Vector[From]): Vector[simple.FieldTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[simple.FieldTerm]]
  implicit  def innerXsO[From <: TermT[Term]](x: Option[From]): Option[simple.Term] =
    x.asInstanceOf[Option[Term]].map(convertToSimple).asInstanceOf[Option[simple.Term]]
  implicit  def innerMap[KeyFrom <: LocalVC[Term], ValueFrom <: TermT[Term]](x: Map[KeyFrom, ValueFrom]): Map[simple.LocalV, simple.Term] =
    x.asInstanceOf[Map[Term, Term]].map((k, v) => (convertToSimple(k), convertToSimple(v))).asInstanceOf[Map[simple.LocalV, simple.Term]]
  term match {
    case x: CallingArgTermC[Term]        => simple.CallingArgTerm(value = x.value, ty = x.ty, name = x.name, vararg = x.vararg, meta = x.meta)
    case x: CallingC[Term]               => simple.Calling(args = x.args, implicitly = x.implicitly, meta = x.meta)
    case x: FCallTermC[Term]             => simple.FCallTerm(f = x.f, args = x.args, meta = x.meta)
    case x: BindC[Term]                  => simple.Bind(bind = x.bind, ty = x.ty, meta = x.meta)
    case x: MetaTermC[Term]              => simple.MetaTerm(impl = x.impl, meta = x.meta)
    case x: ListTermC[Term]              => simple.ListTerm(terms = x.terms, meta = x.meta)
    case x: TypeC[Term]                  => simple.Type(level = x.level, meta = x.meta)
    case x: LevelTypeC[Term]             => simple.LevelType(meta = x.meta)
    case x: LevelFiniteC[Term]           => simple.LevelFinite(n = x.n, meta = x.meta)
    case x: LevelUnrestrictedC[Term]     => simple.LevelUnrestricted(meta = x.meta)
    case x: PropC[Term]                  => simple.Prop(level = x.level, meta = x.meta)
    case x: FTypeC[Term]                 => simple.FType(level = x.level, meta = x.meta)
    case x: IntTermC[Term]               => simple.IntTerm(value = x.value, meta = x.meta)
    case x: IntegerTermC[Term]           => simple.IntegerTerm(value = x.value, meta = x.meta)
    case x: RationalTermC[Term]          => simple.RationalTerm(value = x.value, meta = x.meta)
    case x: BooleanTermC[Term]           => simple.BooleanTerm(value = x.value, meta = x.meta)
    case x: StringTermC[Term]            => simple.StringTerm(value = x.value, meta = x.meta)
    case x: SymbolTermC[Term]            => simple.SymbolTerm(value = x.value, meta = x.meta)
    case x: ArgTermC[Term]               => simple.ArgTerm(bind = x.bind, ty = x.ty, default = x.default, vararg = x.vararg, meta = x.meta)
    case x: TelescopeTermC[Term]         => simple.TelescopeTerm(args = x.args, implicitly = x.implicitly, meta = x.meta)
    case x: FunctionC[Term]              => simple.Function(ty = x.ty, body = x.body, meta = x.meta)
    case x: FunctionTypeC[Term]          => simple.FunctionType(telescope = x.telescope, resultTy = x.resultTy, effects = x.effects, meta = x.meta)
    case x: ObjectTermC[Term]            => simple.ObjectTerm(clauses = x.clauses, meta = x.meta)
    case x: ObjectTypeC[Term]            => simple.ObjectType(fieldTypes = x.fieldTypes, exactFields = x.exactFields, meta = x.meta)
    case x: ListTypeC[Term]              => simple.ListType(ty = x.ty, meta = x.meta)
    case x: UnionC[Term]                 => simple.Union(xs = x.xs, meta = x.meta)
    case x: IntersectionC[Term]          => simple.Intersection(xs = x.xs, meta = x.meta)
    case x: EffectsC[Term]               => simple.Effects(effectss = x.effects, meta = x.meta)
    case x: LocalVC[Term]                => simple.LocalV(name = x.name, ty = x.ty, uniqId = x.uniqId, meta = x.meta)
    case x: ToplevelVC[Term]             => simple.ToplevelV(id = x.id, ty = x.ty, uniqId = x.uniqId, meta = x.meta)
    case x: ErrorTermC[Term]             => simple.ErrorTerm(problem = x.problem, meta = x.meta)
    case x: BlockTermC[Term]             => simple.BlockTerm(statements = x.statements, result = x.result, meta = x.meta)
    case x: AnnotationC[Term]            => simple.Annotation(term = x.term, ty = x.ty, effects = x.effects, meta = x.meta)
    case x: IntegerTypeC[Term]           => simple.IntegerType(meta = x.meta)
    case x: IntTypeC[Term]               => simple.IntType(meta = x.meta)
    case x: UIntTypeC[Term]              => simple.UIntType(meta = x.meta)
    case x: NaturalTypeC[Term]           => simple.NaturalType(meta = x.meta)
    case x: BooleanTypeC[Term]           => simple.BooleanType(meta = x.meta)
    case x: RationalTypeC[Term]          => simple.RationalType(meta = x.meta)
    case x: FloatTypeC[Term]             => simple.FloatType(meta = x.meta)
    case x: StringTypeC[Term]            => simple.StringType(meta = x.meta)
    case x: SymbolTypeC[Term]            => simple.SymbolType(meta = x.meta)
    case x: AnyTypeC[Term]               => simple.AnyType(level = x.level, meta = x.meta)
    case x: NothingTypeC[Term]           => simple.NothingType(meta = x.meta)
    case x: LiteralTypeC[Term]           => simple.LiteralType(literal = x.literal, meta = x.meta)
    case x: ObjectClauseValueTermC[Term] => simple.ObjectClauseValueTerm(key = x.key, value = x.value, meta = x.meta)
    case x: ListFC[Term]                 => simple.ListF(meta = x.meta)
    case x: ExceptionEffectC[Term]       => simple.ExceptionEffect(meta = x.meta)
    case x: LetStmtTermC[Term]           => simple.LetStmtTerm(localv = x.localv, value = x.value, ty = x.ty, meta = x.meta)
    case x: DefStmtTermC[Term]           => simple.DefStmtTerm(localv = x.localv, value = x.value, ty = x.ty, meta = x.meta)
    case x: ExprStmtTermC[Term]          => simple.ExprStmtTerm(expr = x.expr, ty = x.ty, meta = x.meta)
    case x: NonlocalOrLocalReturnC[Term] => simple.NonlocalOrLocalReturn(value = x.value, meta = x.meta)
    case x: TupleTypeC[Term]             => simple.TupleType(types = x.types, meta = x.meta)
    case x: TupleTermC[Term]             => simple.TupleTerm(values = x.values, meta = x.meta)
    case x: FieldTermC[Term]             => simple.FieldTerm(name = x.name, ty = x.ty, meta = x.meta)
    case x: RecordStmtTermC[Term]        => simple.RecordStmtTerm(name = x.name, uniqId = x.uniqId, fields = x.fields, body = x.body, meta = x.meta)
    case x: RecordConstructorCallTermC[Term] => simple.RecordConstructorCallTerm(recordName = x.recordName, args = x.args, meta = x.meta)
    case x: TraitStmtTermC[Term] =>
      simple.TraitStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: InterfaceStmtTermC[Term] =>
      simple.InterfaceStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: ObjectCallTermC[Term] => simple.ObjectCallTerm(objectRef = x.objectRef, meta = x.meta)
    case x: ObjectTypeTermC[Term] => simple.ObjectTypeTerm(objectDef = x.objectDef, meta = x.meta)
    case x: ObjectStmtTermC[Term] =>
      simple.ObjectStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case _ => throw new RuntimeException(s"Unhandled term type in convertToSimple: ${term.getClass}")
  }
}
