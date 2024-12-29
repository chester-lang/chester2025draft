package chester.syntax.core

import cats.data.NonEmptyVector
import chester.syntax.core.spec.spec.*

import scala.language.implicitConversions

def convertToSimple[Term<:TermT[Term]](term: Term): simple.Term = {
  implicit inline def innerConvert[From <: TermT[Term], A <: simple.Term](t: From): A = convertToSimple(t.asInstanceOf[Term]).asInstanceOf[A]
  implicit inline def innerXsN[From <: TermT[Term], A <: simple.Term](x: NonEmptyVector[From]): NonEmptyVector[A] = x.asInstanceOf[NonEmptyVector[Term]].map(convertToSimple).asInstanceOf[NonEmptyVector[A]]
  implicit inline def innerXs[From <: TermT[Term], A <: simple.Term](x: Vector[From]): Vector[A] = x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[A]]
  implicit inline def innerXsO[From <: TermT[Term], A <: simple.Term](x: Option[From]): Option[A] = x.asInstanceOf[Option[Term]].map(convertToSimple).asInstanceOf[Option[A]]
  implicit inline def innerMap[KeyFrom <: TermT[Term],ValueFrom<:TermT[Term],A<:simple.Term,B<:simple.Term](x: Map[KeyFrom, ValueFrom]):Map[A,B] = x.asInstanceOf[Map[Term,Term]].map((k,v) => (convertToSimple(k),convertToSimple(v))).asInstanceOf[Map[A,B]]
  term match {
    case x: CallingArgTermC[Term] => simple.CallingArgTerm(value = x.value, ty = x.ty, name = x.name, vararg = x.vararg, meta = x.meta)
    case x: CallingC[Term] => simple.Calling(args = x.args, implicitly = x.implicitly, meta = x.meta)
    case x: FCallTermC[Term] => simple.FCallTerm(f = x.f, args = x.args, meta = x.meta)
    case x: BindC[Term] => simple.Bind(bind = x.bind, ty = x.ty, meta = x.meta)
    case x: MetaTermC[Term] => simple.MetaTerm(impl = x.impl, meta = x.meta)
    case x: ListTermC[Term] => simple.ListTerm(terms = x.terms, meta = x.meta)
    case x: TypeC[Term] => simple.Type(level = x.level, meta = x.meta)
    case x: LevelTypeC[Term] => simple.LevelType(meta = x.meta)
    case x: LevelFiniteC[Term] => simple.LevelFinite(n = x.n, meta = x.meta)
    case x: LevelUnrestrictedC[Term] => simple.LevelUnrestricted(meta = x.meta)
    case x: PropC[Term] => simple.Prop(level = x.level, meta = x.meta)
    case x: FTypeC[Term] => simple.FType(level = x.level, meta = x.meta)
    case x: IntTermC[Term] => simple.IntTerm(value = x.value, meta = x.meta)
    case x: IntegerTermC[Term] => simple.IntegerTerm(value = x.value, meta = x.meta)
    case x: RationalTermC[Term] => simple.RationalTerm(value = x.value, meta = x.meta)
    case x: BooleanTermC[Term] => simple.BooleanTerm(value = x.value, meta = x.meta)
    case x: StringTermC[Term] => simple.StringTerm(value = x.value, meta = x.meta)
    case x: SymbolTermC[Term] => simple.SymbolTerm(value = x.value, meta = x.meta)
    case x: ArgTermC[Term] => simple.ArgTerm(bind = x.bind, ty = x.ty, default = x.default, vararg = x.vararg, meta = x.meta)
    case x: TelescopeTermC[Term] => simple.TelescopeTerm(args = x.args, implicitly = x.implicitly, meta = x.meta)
    case x: FunctionC[Term] => simple.Function(ty = x.ty, body = x.body, meta = x.meta)
    case x: FunctionTypeC[Term] => simple.FunctionType(telescope = x.telescope, resultTy = x.resultTy, effects = x.effects, meta = x.meta)
    case x: ObjectTermC[Term] => simple.ObjectTerm(clauses = x.clauses, meta = x.meta)
    case x: ObjectTypeC[Term] => simple.ObjectType(fieldTypes = x.fieldTypes, exactFields = x.exactFields, meta = x.meta)
    case x: ListTypeC[Term] => simple.ListType(ty = x.ty, meta = x.meta)
    case x: UnionC[Term] => simple.Union(xs = x.xs, meta = x.meta)
    case x: IntersectionC[Term] => simple.Intersection(xs = x.xs, meta = x.meta)
    case x: EffectsC[Term] => simple.Effects(effectss = x.effects, meta = x.meta)
    case x: LocalVC[Term] => simple.LocalV(name = x.name, ty = x.ty, uniqId = x.uniqId, meta = x.meta)
    case x: ToplevelVC[Term] => simple.ToplevelV(id = x.id, ty = x.ty, uniqId = x.uniqId, meta = x.meta)
    case x: ErrorTermC[Term] => simple.ErrorTerm(problem = x.problem, meta = x.meta)
    case x: BlockTermC[Term] => simple.BlockTerm(statements = x.statements, result = x.result, meta = x.meta)
    case x: AnnotationC[Term] => simple.Annotation(term = x.term, ty = x.ty, effects = x.effects, meta = x.meta)
    case _ => throw new RuntimeException(s"Unhandled term type in convertToSimple: ${term.getClass}")
  }
}