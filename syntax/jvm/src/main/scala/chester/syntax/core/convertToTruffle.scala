package chester.syntax.core

import cats.data.NonEmptyVector
import chester.syntax.core.spec.spec.*

import scala.language.implicitConversions

def convertToTruffle[Term <: TermT[Term]](term: Term): truffle.Term = {
  implicit def convertLiteralTermT[From <: LiteralTermT[Term]](t: From): truffle.LiteralTerm =
    convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.LiteralTerm]
  implicit def innerConvert[From <: TermT[Term]](t: From): truffle.Term = convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.Term]
  implicit def innerCOnvertLocalV[From <: LocalVC[Term]](t: From): truffle.LocalV =
    convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.LocalV]
  implicit def innerConvertyRecordStmtTerm[From <: RecordStmtTermC[Term]](t: From): truffle.RecordStmtTerm =
    convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.RecordStmtTerm]
  implicit def innerConvertTelescopeTerm[From <: TelescopeTermC[Term]](t: From): truffle.TelescopeTerm =
    convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.TelescopeTerm]

  implicit def innerConvertFunctionType[From <: FunctionTypeC[Term]](t: From): truffle.FunctionType =
    convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.FunctionType]
  implicit def innerConvertEffects[From <: EffectsMT[Term]](t: From): truffle.EffectsM =
    convertToTruffle(t.asInstanceOf[Term]).asInstanceOf[truffle.EffectsM]
  implicit def innerXsN[From <: TermT[Term]](x: NonEmptyVector[From]): NonEmptyVector[truffle.Term] =
    x.asInstanceOf[NonEmptyVector[Term]].map(convertToTruffle).asInstanceOf[NonEmptyVector[truffle.Term]]
  implicit def innerXs[From <: TermT[Term]](x: Vector[From]): Vector[truffle.Term] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.Term]]
  implicit def innerXs2ss[From <: CallingArgTermC[Term]](x: Seq[From]): Seq[truffle.CallingArgTerm] =
    x.asInstanceOf[Seq[Term]].map(convertToTruffle).asInstanceOf[Seq[truffle.CallingArgTerm]]

  implicit def innerXsCallingff[From <: CallingC[Term]](x: Vector[From]): Vector[truffle.Calling] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.Calling]]
  implicit def innerXSTelescope[From <: TelescopeTermC[Term]](x: Vector[From]): Vector[truffle.TelescopeTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.TelescopeTerm]]
  implicit def innerXsArgTerm[From <: ArgTermC[Term]](x: Vector[From]): Vector[truffle.ArgTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.ArgTerm]]
  implicit def innerXsObjectClauseValueTerm[From <: ObjectClauseValueTermC[Term]](x: Vector[From]): Vector[truffle.ObjectClauseValueTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.ObjectClauseValueTerm]]
  implicit def innerXsStmtTerm[From <: StmtTermT[Term]](x: Vector[From]): Vector[truffle.StmtTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.StmtTerm]]
  implicit def innerXsFieldTerm[From <: FieldTermC[Term]](x: Vector[From]): Vector[truffle.FieldTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToTruffle).asInstanceOf[Vector[truffle.FieldTerm]]
  implicit def innerXsO[From <: TermT[Term]](x: Option[From]): Option[truffle.Term] =
    x.asInstanceOf[Option[Term]].map(convertToTruffle).asInstanceOf[Option[truffle.Term]]
  implicit def innerMap[KeyFrom <: LocalVC[Term], ValueFrom <: TermT[Term]](x: Map[KeyFrom, ValueFrom]): Map[truffle.LocalV, truffle.Term] =
    x.asInstanceOf[Map[Term, Term]].map((k, v) => (convertToTruffle(k), convertToTruffle(v))).asInstanceOf[Map[truffle.LocalV, truffle.Term]]
  term match {
    case x: CallingArgTermC[Term]        => truffle.CallingArgTerm(value = x.value, ty = x.ty, name = x.name, vararg = x.vararg, meta = x.meta)
    case x: CallingC[Term]               => truffle.Calling(args = x.args, implicitly = x.implicitly, meta = x.meta)
    case x: FCallTermC[Term]             => truffle.FCallTerm(f = x.f, args = x.args, meta = x.meta)
    case x: BindC[Term]                  => truffle.Bind(bind = x.bind, ty = x.ty, meta = x.meta)
    case x: MetaTermC[Term]              => truffle.MetaTerm(impl = x.impl, meta = x.meta)
    case x: ListTermC[Term]              => truffle.ListTerm(terms = x.terms, meta = x.meta)
    case x: TypeC[Term]                  => truffle.Type(level = x.level, meta = x.meta)
    case x: LevelTypeC[Term]             => truffle.LevelType(meta = x.meta)
    case x: LevelFiniteC[Term]           => truffle.LevelFinite(n = x.n, meta = x.meta)
    case x: LevelUnrestrictedC[Term]     => truffle.LevelUnrestricted(meta = x.meta)
    case x: PropC[Term]                  => truffle.Prop(level = x.level, meta = x.meta)
    case x: FTypeC[Term]                 => truffle.FType(level = x.level, meta = x.meta)
    case x: IntTermC[Term]               => truffle.IntTerm(value = x.value, meta = x.meta)
    case x: IntegerTermC[Term]           => truffle.IntegerTerm(value = x.value, meta = x.meta)
    case x: RationalTermC[Term]          => truffle.RationalTerm(value = x.value, meta = x.meta)
    case x: BooleanTermC[Term]           => truffle.BooleanTerm(value = x.value, meta = x.meta)
    case x: StringTermC[Term]            => truffle.StringTerm(value = x.value, meta = x.meta)
    case x: SymbolTermC[Term]            => truffle.SymbolTerm(value = x.value, meta = x.meta)
    case x: ArgTermC[Term]               => truffle.ArgTerm(bind = x.bind, ty = x.ty, default = x.default, vararg = x.vararg, meta = x.meta)
    case x: TelescopeTermC[Term]         => truffle.TelescopeTerm(args = x.args, implicitly = x.implicitly, meta = x.meta)
    case x: FunctionC[Term]              => truffle.Function(ty = x.ty, body = x.body, meta = x.meta)
    case x: FunctionTypeC[Term]          => truffle.FunctionType(telescope = x.telescope, resultTy = x.resultTy, effects = x.effects, meta = x.meta)
    case x: ObjectTermC[Term]            => truffle.ObjectTerm(clauses = x.clauses, meta = x.meta)
    case x: ObjectTypeC[Term]            => truffle.ObjectType(fieldTypes = x.fieldTypes, exactFields = x.exactFields, meta = x.meta)
    case x: ListTypeC[Term]              => truffle.ListType(ty = x.ty, meta = x.meta)
    case x: UnionC[Term]                 => truffle.Union(xs = x.xs, meta = x.meta)
    case x: IntersectionC[Term]          => truffle.Intersection(xs = x.xs, meta = x.meta)
    case x: EffectsC[Term]               => truffle.Effects(effectss = x.effects, meta = x.meta)
    case x: LocalVC[Term]                => truffle.LocalV(name = x.name, ty = x.ty, uniqId = x.uniqId, meta = x.meta)
    case x: ToplevelVC[Term]             => truffle.ToplevelV(id = x.id, ty = x.ty, uniqId = x.uniqId, meta = x.meta)
    case x: ErrorTermC[Term]             => truffle.ErrorTerm(problem = x.problem, meta = x.meta)
    case x: BlockTermC[Term]             => truffle.BlockTerm(statements = x.statements, result = x.result, meta = x.meta)
    case x: AnnotationC[Term]            => truffle.Annotation(term = x.term, ty = x.ty, effects = x.effects, meta = x.meta)
    case x: IntegerTypeC[Term]           => truffle.IntegerType(meta = x.meta)
    case x: IntTypeC[Term]               => truffle.IntType(meta = x.meta)
    case x: UIntTypeC[Term]              => truffle.UIntType(meta = x.meta)
    case x: NaturalTypeC[Term]           => truffle.NaturalType(meta = x.meta)
    case x: BooleanTypeC[Term]           => truffle.BooleanType(meta = x.meta)
    case x: RationalTypeC[Term]          => truffle.RationalType(meta = x.meta)
    case x: FloatTypeC[Term]             => truffle.FloatType(meta = x.meta)
    case x: StringTypeC[Term]            => truffle.StringType(meta = x.meta)
    case x: SymbolTypeC[Term]            => truffle.SymbolType(meta = x.meta)
    case x: AnyTypeC[Term]               => truffle.AnyType(level = x.level, meta = x.meta)
    case x: NothingTypeC[Term]           => truffle.NothingType(meta = x.meta)
    case x: LiteralTypeC[Term]           => truffle.LiteralType(literal = x.literal, meta = x.meta)
    case x: ObjectClauseValueTermC[Term] => truffle.ObjectClauseValueTerm(key = x.key, value = x.value, meta = x.meta)
    case x: ListFC[Term]                 => truffle.ListF(meta = x.meta)
    case x: ExceptionEffectC[Term]       => truffle.ExceptionEffect(meta = x.meta)
    case x: LetStmtTermC[Term]           => truffle.LetStmtTerm(localv = x.localv, value = x.value, ty = x.ty, meta = x.meta)
    case x: DefStmtTermC[Term]           => truffle.DefStmtTerm(localv = x.localv, value = x.value, ty = x.ty, meta = x.meta)
    case x: ExprStmtTermC[Term]          => truffle.ExprStmtTerm(expr = x.expr, ty = x.ty, meta = x.meta)
    case x: NonlocalOrLocalReturnC[Term] => truffle.NonlocalOrLocalReturn(value = x.value, meta = x.meta)
    case x: TupleTypeC[Term]             => truffle.TupleType(types = x.types, meta = x.meta)
    case x: TupleTermC[Term]             => truffle.TupleTerm(values = x.values, meta = x.meta)
    case x: FieldTermC[Term]             => truffle.FieldTerm(name = x.name, ty = x.ty, meta = x.meta)
    case x: RecordStmtTermC[Term]        => truffle.RecordStmtTerm(name = x.name, uniqId = x.uniqId, fields = x.fields, body = x.body, extendsClause = x.extendsClause, meta = x.meta)
    case x: RecordConstructorCallTermC[Term] => truffle.RecordConstructorCallTerm(recordName = x.recordName, args = x.args, meta = x.meta)
    case x: TraitStmtTermC[Term] =>
      truffle.TraitStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: InterfaceStmtTermC[Term] =>
      truffle.InterfaceStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: ObjectCallTermC[Term] => truffle.ObjectCallTerm(objectRef = x.objectRef, meta = x.meta)
    case x: ObjectTypeTermC[Term] => truffle.ObjectTypeTerm(objectDef = x.objectDef, meta = x.meta)
    case x: TraitCallTermC[Term] => truffle.TraitCallTerm(traitDef = x.traitDef, meta = x.meta)
    case x: ObjectStmtTermC[Term] =>
      truffle.ObjectStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: RecordCallTermC[Term]  => truffle.RecordCallTerm(recordDef = x.recordDef, telescope = x.telescope, meta = x.meta)
    case x: FieldAccessTermC[Term] => truffle.FieldAccessTerm(record = x.record, fieldName = x.fieldName, fieldType = x.fieldType, meta = x.meta)
    case _                         => throw new RuntimeException(s"Unhandled term type in convertToTruffle: ${term.getClass}")
  }
}
