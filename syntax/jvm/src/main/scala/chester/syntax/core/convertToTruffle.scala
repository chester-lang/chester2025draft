package chester.syntax.core

import cats.data.NonEmptyVector
import chester.syntax.core.spec.spec.*

import scala.language.implicitConversions

def convertToTruffle[Term <: TermT[Term]](term: Term): truffle.Term = {
  implicit inline def convertLiteralTermT[From <: LiteralTermT[Term]](t: From): truffle.LiteralTerm =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[truffle.LiteralTerm]
  implicit inline def innerConvert[From <: TermT[Term]](t: From): truffle.Term = convertToSimple(t.asInstanceOf[Term]).asInstanceOf[truffle.Term]
  implicit inline def innerCOnvertLocalV[From <: LocalVC[Term]](t: From): truffle.LocalV =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[truffle.LocalV]
  
  implicit inline def innerConvertFunctionType[From <: FunctionTypeC[Term]](t: From): truffle.FunctionType =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[truffle.FunctionType]
  implicit inline def innerConvertEffects[From <: EffectsMT[Term]](t: From): truffle.EffectsM =
    convertToSimple(t.asInstanceOf[Term]).asInstanceOf[truffle.EffectsM]
  implicit inline def innerXsN[From <: TermT[Term]](x: NonEmptyVector[From]): NonEmptyVector[truffle.Term] =
    x.asInstanceOf[NonEmptyVector[Term]].map(convertToSimple).asInstanceOf[NonEmptyVector[truffle.Term]]
  implicit inline def innerXs[From <: TermT[Term]](x: Vector[From]): Vector[truffle.Term] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.Term]]
  implicit inline def innerXs2[From <: CallingArgTermC[Term]](x: Vector[From]): Vector[truffle.CallingArgTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.CallingArgTerm]]
  implicit inline def innerXsCalling[From <: CallingC[Term]](x: Vector[From]): Vector[truffle.Calling] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.Calling]]
  implicit inline def innerXSTelescope[From <: TelescopeTermC[Term]](x: Vector[From]): Vector[truffle.TelescopeTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.TelescopeTerm]]
  implicit inline def innerXsArgTerm[From <: ArgTermC[Term]](x: Vector[From]): Vector[truffle.ArgTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.ArgTerm]]
  implicit inline def innerXsObjectClauseValueTerm[From <: ObjectClauseValueTermC[Term]](x: Vector[From]): Vector[truffle.ObjectClauseValueTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.ObjectClauseValueTerm]]
  implicit inline def innerXsStmtTerm[From <: StmtTermT[Term]](x: Vector[From]): Vector[truffle.StmtTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.StmtTerm]]
  implicit inline def innerXsFieldTerm[From <: FieldTermC[Term]](x: Vector[From]): Vector[truffle.FieldTerm] =
    x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[truffle.FieldTerm]]
  implicit inline def innerXsO[From <: TermT[Term]](x: Option[From]): Option[truffle.Term] =
    x.asInstanceOf[Option[Term]].map(convertToSimple).asInstanceOf[Option[truffle.Term]]
  implicit inline def innerMap[KeyFrom <: LocalVC[Term], ValueFrom <: TermT[Term]](x: Map[KeyFrom, ValueFrom]): Map[truffle.LocalV, truffle.Term] =
    x.asInstanceOf[Map[Term, Term]].map((k, v) => (convertToSimple(k), convertToSimple(v))).asInstanceOf[Map[truffle.LocalV, truffle.Term]]
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
    case x: RecordStmtTermC[Term]        => truffle.RecordStmtTerm(name = x.name, uniqId = x.uniqId, fields = x.fields, body = x.body, meta = x.meta)
    case x: RecordConstructorCallTermC[Term] => truffle.RecordConstructorCallTerm(recordName = x.recordName, args = x.args, meta = x.meta)
    case x: TraitStmtTermC[Term] =>
      truffle.TraitStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: InterfaceStmtTermC[Term] =>
      truffle.InterfaceStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case x: ObjectCallTermC[Term] => truffle.ObjectCallTerm(objectRef = x.objectRef, meta = x.meta)
    case x: ObjectTypeTermC[Term] => truffle.ObjectTypeTerm(objectDef = x.objectDef, meta = x.meta)
    case x: ObjectStmtTermC[Term] =>
      truffle.ObjectStmtTerm(name = x.name, uniqId = x.uniqId, extendsClause = x.extendsClause, body = x.body, meta = x.meta)
    case _ => throw new RuntimeException(s"Unhandled term type in convertToSimple: ${term.getClass}")
  }
}
