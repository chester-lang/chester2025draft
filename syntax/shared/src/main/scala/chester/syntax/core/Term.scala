// TODO: More correctly implement toDoc
package chester.syntax.core

import cats.data.*
import chester.doc.*
import chester.doc.consts.{ColorProfile, Docs}
import chester.error.*
import chester.syntax.*
import chester.uniqid.*
import chester.utils.{*, given}
import chester.utils.doc.*
import spire.math.Trilean.*
import spire.math.{Rational, Trilean}
import upickle.default.*
import chester.error.ProblemUpickle.problemRW
import chester.utils.impls.rationalRW
import com.oracle.truffle.api.frame.VirtualFrame
import spire.math.*
import chester.utils.impls.uintRW

import scala.language.implicitConversions
import scala.collection.immutable.HashMap
import scala.collection.immutable.ArraySeq

case class TermMeta(sourcePos: SourcePos) derives ReadWriter

type ExecuteGeneric = (VirtualFrame, Term) => Object
val globalExecuteGeneric: Parameter[ExecuteGeneric] = new Parameter[ExecuteGeneric]

sealed abstract class Term extends com.oracle.truffle.api.nodes.Node with ToDoc with WithPos with ContainsUniqid with Tree[Term] derives ReadWriter {
  type ThisTree <: Term
  final def executeGeneric(frame: VirtualFrame): Object = globalExecuteGeneric.get(frame, this)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term

  def meta: Option[TermMeta]

  def whnf: Trilean

  def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)

  private def doElevate(level: IntegerTerm): Term = descent(_.doElevate(level))

  final def elevate(level: IntegerTerm): Term = {
    require(level.value >= 0)
    if (level.value == 0) this else doElevate(level)
  }

  // TODO: optimize
  final def substitute[A <: TermWithUniqid](mapping: Seq[(A, Term)]): Term =
    mapping.foldLeft(this) { case (acc, (from, to)) =>
      acc.substitute(from, to)
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
    inspect(x => result ++= x.collectMeta)
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

  override final def collectU(collector: UCollector): Unit = inspectRecursive {
    case x: TermWithUniqid => collector(x.uniqId)
    case _                 =>
  }

  override final def replaceU(reranger: UReplacer): Term = descentRecursive {
    case x: TermWithUniqid => x.switchUniqId(reranger)
    case x                 => x
  }
}
case class CallingArgTerm(
    @child var value: Term,
    @child var ty: Term,
    @const name: Option[Name] = None,
    @const vararg: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = CallingArgTerm

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(value = f(value), ty = f(ty))
  )

  override def toDoc(using PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

}

implicit val SeqCallingArgTermRW: ReadWriter[Seq[CallingArgTerm]] =
  readwriter[Seq[Term]].asInstanceOf[ReadWriter[Seq[CallingArgTerm]]]

implicit inline def makeArray[T: scala.reflect.ClassTag](xs: Seq[T]): Array[T] = xs.toArray

case class Calling(
    @children args0: Array[CallingArgTerm],
    @const implicitly: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {

  val args: Seq[CallingArgTerm] = ArraySeq.unsafeWrapArray(args0)

  override def toDoc(using PrettierOptions): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(args0 = args.map(g))
  )

  override type ThisTree = Calling

}

case class FCallTerm(
    @child var f: Term,
    @children args0: Array[Calling],
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  val args: ArraySeq[Calling] = ArraySeq.unsafeWrapArray(args0)
  override type ThisTree = FCallTerm

  override def descent(a: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(f = a(f), args0 = args.map(g))
  )
  override def toDoc(using PrettierOptions): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

}
sealed abstract class Pat extends SpecialTerm derives ReadWriter {
  override type ThisTree <: Pat
}
case class Bind(
    @child var bind: LocalV,
    @child var ty: Term,
    @const meta: Option[TermMeta]
) extends Pat derives ReadWriter {
  override type ThisTree = Bind
  override def toDoc(using PrettierOptions): Doc = bind.toDoc <+> Docs.`:` <+> ty.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(bind = g(bind), ty = f(ty)))
}
sealed trait WHNF extends Term derives ReadWriter {
  override type ThisTree <: WHNF
  override def whnf: Trilean = True
}
sealed abstract class Uneval extends Term derives ReadWriter {
  override type ThisTree <: Uneval
  override def whnf: Trilean = False
}
sealed trait SpecialTerm extends Term derives ReadWriter {
  override type ThisTree <: SpecialTerm
  override def whnf: Trilean = Unknown
}
sealed trait TermWithUniqid extends Term with HasUniqid derives ReadWriter {
  override type ThisTree <: TermWithUniqid

  override def uniqId: UniqidOf[Term]

  def switchUniqId(r: UReplacer): TermWithUniqid
}
extension (e: EffectsM) {
  def nonEmpty: Boolean = e match {
    case e: Effects => e.nonEmpty
    case _          => true
  }
}

sealed abstract class EffectsM extends Term derives ReadWriter {
  override type ThisTree <: EffectsM
}
case class MetaTerm(@const impl: InMeta[?], @const meta: Option[TermMeta]) extends EffectsM with SpecialTerm derives ReadWriter {
  override type ThisTree = MetaTerm

  override def toDoc(using PrettierOptions): Doc =
    Doc.group("Meta" <> Doc.text(impl.toString))

  def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class ListTerm(@children terms0: Array[Term], @const meta: Option[TermMeta]) extends WHNF derives ReadWriter {
  val terms: ArraySeq[Term] = ArraySeq.unsafeWrapArray(terms0)
  override final type ThisTree = ListTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(terms0 = terms.map(f))
  )
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`[`, Docs.`]`)(terms)
}
sealed trait TypeTerm extends WHNF derives ReadWriter {
  override type ThisTree <: TypeTerm
}
sealed trait SimpleType extends WHNF derives ReadWriter {
  override type ThisTree <: SimpleType
}
sealed abstract class Sort extends TypeTerm derives ReadWriter {
  override type ThisTree <: Sort
  def level: Term
}
case class Type(@child var level: Term, @const meta: Option[TermMeta]) extends Sort derives ReadWriter {
  override type ThisTree = Type

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist("Type" <> Docs.`(`, Docs.`)`)(Vector(level))
}
case class LevelType(@const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = LevelType
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("LevelType")
}
sealed abstract class Level extends WHNF derives ReadWriter {
  type ThisTree <: Level
}
case class LevelFinite(@child var n: Term, @const meta: Option[TermMeta]) extends Level derives ReadWriter {
  override type ThisTree = LevelFinite
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(n = f(n)))
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Level(") <> n.toDoc <> Doc.text(")")
}
case class LevelUnrestricted(@const meta: Option[TermMeta]) extends Level derives ReadWriter {
  override type ThisTree = LevelUnrestricted
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Levelω")
}

enum Usage derives ReadWriter {
  case None, Linear, Unrestricted
}

enum Coeffect derives ReadWriter {
  case Usage
}
case class Prop(@child var level: Term, @const meta: Option[TermMeta]) extends Sort derives ReadWriter {
  override type ThisTree = Prop
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist("Prop" <> Docs.`(`, Docs.`)`)(Vector(level))
}
// fibrant types
case class FType(@child var level: Term, @const meta: Option[TermMeta]) extends Sort derives ReadWriter {
  override type ThisTree = FType
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(level = f(level)))

  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
}
sealed abstract class LiteralTerm extends WHNF derives ReadWriter {
  override type ThisTree <: LiteralTerm
}
sealed abstract class AbstractIntTerm extends LiteralTerm derives ReadWriter {
  override type ThisTree <: AbstractIntTerm
}
case class IntTerm(@const value: Int, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = IntTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class UIntTerm(@const value: UInt, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = UIntTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class IntegerTerm(@const value: BigInt, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = IntegerTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class NaturalTerm(@const value: BigInt, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = NaturalTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class IntegerType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = IntegerType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Integer", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
// int of 32 bits or more
case class IntType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = IntType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Int", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
// unsigned int of 32 bits or more
case class UIntType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = UIntType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class NaturalType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = NaturalType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class RationalTerm(@const value: Rational, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = RationalTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class BooleanTerm(@const value: Boolean, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = BooleanTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class BooleanType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = BooleanType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Boolean", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class StringTerm(@const value: String, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = StringTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class SymbolTerm(@const value: String, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = SymbolTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("'" + value, ColorProfile.literalColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class RationalType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = RationalType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Rational", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
// float of 32 bits or more
case class FloatType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = FloatType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Float", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class StringType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = StringType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("String", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class SymbolType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = SymbolType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Symbol", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class AnyType(@child var level: Term, @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = AnyType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Any", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}

val Level0: LevelFinite = LevelFinite(IntegerTerm(0, meta = None), meta = None)
val Type0: Type = Type(Level0, meta = None)
val AnyType0: AnyType = AnyType(Level0, meta = None)
case class NothingType(@const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = NothingType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("Nothing", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class LiteralType(
    @child var literal: LiteralTerm,
    @const meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = LiteralType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(literal.toString, ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
case class ArgTerm(
    @child var bind: LocalV,
    @child var ty: Term,
    @const default: Option[Term] = None,
    @const vararg: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = ArgTerm
  override def toDoc(using PrettierOptions): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
    bind.toDoc <> varargDoc <> Docs.`:` <+> ty.toDoc <> defaultDoc
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(bind = g(bind), ty = f(ty), default = default.map(f))
  )

  def name: Name = bind.name
}
case class TelescopeTerm(
    @children args0: Array[ArgTerm],
    @const implicitly: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  val args: ArraySeq[ArgTerm] = ArraySeq.unsafeWrapArray(args0)
  override type ThisTree = TelescopeTerm
  override def toDoc(using PrettierOptions): Doc = {
    val argsDoc =
      args.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    if (implicitly) {
      Docs.`[` <> argsDoc <> Docs.`]`
    } else {
      Docs.`(` <> argsDoc <> Docs.`)`
    }
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(args0 = args.map(g))
  )
}
case class Function(
    @child var ty: FunctionType,
    @child var body: Term,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = Function
  override def toDoc(using PrettierOptions): Doc = {
    val paramsDoc = ty.telescopes.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val returnTypeDoc = Docs.`:` <+> ty.resultTy.toDoc
    val effectsDoc = if (ty.effects.nonEmpty) {
      Docs.`/` <+> ty.effects.toDoc
    } else {
      Doc.empty
    }
    val bodyDoc = body.toDoc
    group(paramsDoc <> returnTypeDoc <+> Docs.`=>` <+> bodyDoc <> effectsDoc)
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(ty = g(ty), body = f(body)))
}
case class FunctionType(
    @children telescopes0: Array[TelescopeTerm],
    @child var resultTy: Term,
    @const effects: EffectsM = Effects.Empty,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  val telescopes: ArraySeq[TelescopeTerm] = ArraySeq.unsafeWrapArray(telescopes0)
  override type ThisTree = FunctionType
  override def toDoc(using PrettierOptions): Doc = {
    val telescopeDoc =
      telescopes.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val effectsDoc = if (effects.nonEmpty) {
      Docs.`/` <+> effects.toDoc
    } else {
      Doc.empty
    }
    group(telescopeDoc <+> Docs.`->` <+> resultTy.toDoc <> effectsDoc)
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(
      telescopes0 = telescopes.map(g),
      resultTy = f(resultTy),
      effects = g(effects)
    )
  )
}
case class ObjectClauseValueTerm(
    @child var key: Term,
    @child var value: Term,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = ObjectClauseValueTerm
  override def toDoc(using PrettierOptions): Doc = group(
    key.toDoc <+> Doc.text("=") <+> value.toDoc
  )

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(key = f(key), value = f(value))
  )
}
case class ObjectTerm(
    @children clauses0: Array[ObjectClauseValueTerm],
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  val clauses: ArraySeq[ObjectClauseValueTerm] = ArraySeq.unsafeWrapArray(clauses0)
  override type ThisTree = ObjectTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`)(clauses.map(_.toDoc))

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(clauses0 = clauses.map(g))
  )
}
// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
case class ObjectType(
    @children fieldTypes0: Array[ObjectClauseValueTerm],
    @const exactFields: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  private val fieldTypes: ArraySeq[ObjectClauseValueTerm] = ArraySeq.unsafeWrapArray(fieldTypes0)
  override type ThisTree = ObjectType
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`)(
      fieldTypes.map(_.toDoc)
    )
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(fieldTypes0 = fieldTypes.map(g))
  )
}
case class ListF(@const meta: Option[TermMeta]) extends Builtin derives ReadWriter {
  override type ThisTree = ListF
  override def toDoc(using PrettierOptions): Doc = "List"
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
sealed abstract class Constructed extends WHNF derives ReadWriter {
  type ThisTree <: Constructed
}
case class ListType(@child var ty: Term, @const meta: Option[TermMeta]) extends Constructed with TypeTerm derives ReadWriter {
  override type ThisTree = ListType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("List") <> Docs.`(` <> ty <> Docs.`)`
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(ty = f(ty)))
}
case class Union(@const xs: NonEmptyVector[Term], @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = Union
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, "|")(xs)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(xs = xs.map(f))
  )
}
case class Intersection(@const xs: NonEmptyVector[Term], @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = Intersection
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, "&")(xs)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(xs = xs.map(f)))
}
sealed abstract class Builtin extends WHNF derives ReadWriter {
  override type ThisTree <: Builtin
}
sealed abstract class Effect extends WHNF derives ReadWriter {
  override type ThisTree <: Effect
}
case class Effects(@const effects: Map[LocalV, Term] = HashMap.empty, @const meta: Option[TermMeta]) extends EffectsM with WHNF derives ReadWriter {

  override type ThisTree = Effects
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`)(effects.map { case (k, v) =>
      k.toDoc <+> Docs.`:` <+> v.toDoc
    })

  override def collectMeta: Vector[MetaTerm] =
    effects.flatMap((a, b) => a.collectMeta ++ b.collectMeta).toVector

  override def replaceMeta(f: MetaTerm => Term): Term = copy(effects = effects.map { case (a, b) =>
    (a.replaceMeta(f).asInstanceOf[LocalV], b.replaceMeta(f))
  })
  def isEmpty: Boolean = effects.isEmpty

  def nonEmpty: Boolean = effects.nonEmpty

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(effects = effects.map { case (k, v) => (k, f(v)) }))
}
object Effects {
  val Empty: Effects = Effects(Map.empty, meta = None)
}

/** Effect needs to have reasonable equals and hashcode for simple comparison, whereas they are not requirements for other Terms
  */
// may raise an exception
case class ExceptionEffect(@const meta: Option[TermMeta]) extends Effect derives ReadWriter {
  override type ThisTree = ExceptionEffect
  val name = "Exception"
  override def toDoc(using PrettierOptions): Doc = Doc.text(name)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
// todo: may not terminate

// todo:  whatever IO: console, file, network, ...
sealed abstract class ReferenceCall extends Uneval with TermWithUniqid derives ReadWriter {
  override type ThisTree <: ReferenceCall
  def name: Name

  def ty: Term

}
case class LocalV(
    @const name: Name,
    @child var ty: Term,
    @const uniqId: UniqidOf[LocalV],
    @const meta: Option[TermMeta]
) extends ReferenceCall derives ReadWriter {
  override type ThisTree = LocalV
  override def toDoc(using PrettierOptions): Doc = Doc.text(name)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(ty = f(ty)))
  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))

}
case class ToplevelV(
    @const id: AbsoluteRef,
    @child var ty: Term,
    @const uniqId: UniqidOf[ToplevelV],
    @const meta: Option[TermMeta]
) extends ReferenceCall derives ReadWriter {
  override type ThisTree = ToplevelV
  override def toDoc(using PrettierOptions): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(copy(ty = f(ty)))

  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))
  @deprecated("dont use")
  def name: Name = id.name
}
case class ErrorTerm(@const problem: Problem, @const meta: Option[TermMeta]) extends SpecialTerm derives ReadWriter {
  override type ThisTree = ErrorTerm
  override def toDoc(using PrettierOptions): Doc = problem.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
}
sealed abstract class StmtTerm extends Term with SpecialTerm derives ReadWriter {
  override type ThisTree <: StmtTerm
}
case class LetStmtTerm(
    @child var localv: LocalV,
    @child var value: Term,
    @child var ty: Term,
    @const meta: Option[TermMeta]
) extends StmtTerm derives ReadWriter {
  override type ThisTree = LetStmtTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}
case class DefStmtTerm(
    @child var localv: LocalV,
    @child var value: Term,
    @child var ty: Term,
    @const meta: Option[TermMeta]
) extends StmtTerm derives ReadWriter {
  override type ThisTree = DefStmtTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(
      localv = g(localv),
      value = f(value),
      ty = f(ty)
    )
  )
}
case class ExprStmtTerm(
    @child var expr: Term,
    @child var ty: Term = AnyType0,
    @const meta: Option[TermMeta]
) extends StmtTerm derives ReadWriter {
  override type ThisTree = ExprStmtTerm
  override def toDoc(using PrettierOptions): Doc = expr.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(expr = f(expr), ty = f(ty))
  )
}
case class NonlocalOrLocalReturn(@child var value: Term, @const meta: Option[TermMeta]) extends StmtTerm derives ReadWriter {
  override type ThisTree = NonlocalOrLocalReturn
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("return") <+> value.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(value = f(value))
  )
}
case class TupleType(@const types: Vector[Term], @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = TupleType
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`)(types)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(types = types.map(f))
  )
}
case class TupleTerm(@const values: Vector[Term], @const meta: Option[TermMeta]) extends WHNF derives ReadWriter {
  override type ThisTree = TupleTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`)(values)
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(values = values.map(f))
  )
}
case class BlockTerm(
    @children statements0: Array[StmtTerm],
    @child var result: Term,
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  val statements: ArraySeq[StmtTerm] = ArraySeq.unsafeWrapArray(statements0)
  override type ThisTree = BlockTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, ";")(
      statements.map(_.toDoc) :+ result.toDoc
    )
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(
      statements0 = statements.map(g),
      result = f(result)
    )
  )
}
case class Annotation(
    @child var term: Term,
    @const ty: Option[Term],
    @const effects: Option[EffectsM],
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  require(ty.nonEmpty || effects.nonEmpty)
  override type ThisTree = Annotation
  override def toDoc(using PrettierOptions): Doc = {
    val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
    val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
    term.toDoc <> tyDoc <> effectsDoc
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(g)
    )
  )
}
case class FieldTerm(
    @const name: Name,
    @child var ty: Term,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = FieldTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text(name) <> Doc.text(": ") <> ty.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(ty = f(ty))
  )
}
case class RecordStmtTerm(
    @const name: Name,
    @const uniqId: UniqidOf[RecordStmtTerm] = Uniqid.generate[RecordStmtTerm],
    @const fields: Vector[FieldTerm],
    @const body: Option[BlockTerm],
    @const extendsClause: Option[Term] = None,
    @const meta: Option[TermMeta]
) extends TypeDefinition derives ReadWriter {
  override def names: NonEmptyVector[Name] = NonEmptyVector.of(name)
  override type ThisTree = RecordStmtTerm
  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))

  override def toDoc(using PrettierOptions): Doc = {
    val fieldsDoc = fields.map(_.toDoc).reduceOption(_ <> Doc.text(", ") <> _).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).reduceOption(_ </> _).getOrElse(Doc.empty)

    group(
      Doc.text("record ") <> Doc.text(name) <>
        Doc.group(Doc.text("(") <> fieldsDoc <> Doc.text(")")) <>
        bodyDoc
    )
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(
      fields = fields.map(g),
      body = body.map(g),
      extendsClause = extendsClause.map(f)
    )
  )
}
case class RecordConstructTerm(
    @const recordName: Name,
    @const args: Vector[Term],
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  override type ThisTree = RecordConstructTerm
  override def toDoc(using PrettierOptions): Doc = {
    val argsDoc = Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,`)(args.map(_.toDoc))
    Doc.text(recordName) <> argsDoc
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(args = args.map(f))
  )
}
case class TraitStmtTerm(
    @const name: Name,
    @const uniqId: UniqidOf[TraitStmtTerm] = Uniqid.generate[TraitStmtTerm],
    @const extendsClause: Option[Term] = None,
    @const body: Option[BlockTerm] = None,
    @const meta: Option[TermMeta]
) extends TypeDefinition derives ReadWriter {
  override def names: NonEmptyVector[Name] = NonEmptyVector.of(name)
  override type ThisTree = TraitStmtTerm
  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))

  override def toDoc(using PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("trait ") <> Doc.text(name) <> extendsDoc <> bodyDoc
    )
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(extendsClause = extendsClause.map(g), body = body.map(g))
  )
}
case class InterfaceStmtTerm(
    @const name: Name,
    @const uniqId: UniqidOf[InterfaceStmtTerm] = Uniqid.generate[InterfaceStmtTerm],
    @const extendsClause: Option[Term] = None,
    @const body: Option[BlockTerm] = None,
    @const meta: Option[TermMeta]
) extends TypeDefinition derives ReadWriter {
  override def names: NonEmptyVector[Name] = NonEmptyVector.of(name)
  override type ThisTree = InterfaceStmtTerm
  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))

  override def toDoc(using PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("interface ") <> Doc.text(name) <> extendsDoc <> bodyDoc
    )
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(extendsClause = extendsClause.map(g), body = body.map(g))
  )
}
case class RecordTypeTerm(
    recordDef: RecordStmtTerm,
    telescope: TelescopeTerm,
    meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = RecordTypeTerm
  override def toDoc(using PrettierOptions): Doc =
    group("RecordCall" <+> recordDef.toDoc <> telescope.toDoc)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(recordDef = g(recordDef), telescope = g(telescope))
  )
}
case class TraitTypeTerm(
    @child var traitDef: TraitStmtTerm,
    @const meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = TraitTypeTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("TraitCall(") <> traitDef.name.toDoc <> Doc.text(")")

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(traitDef = g(traitDef))
  )
}
case class ObjectConstructTerm(
    @child var objectRef: Term,
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  override type ThisTree = ObjectConstructTerm
  override def toDoc(using PrettierOptions): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(objectRef = f(objectRef))
  )
}
case class ObjectTypeTerm(
    @child var objectDef: ObjectStmtTerm,
    @const meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = ObjectTypeTerm
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(objectDef = g(objectDef))
  )
}
case class ObjectStmtTerm(
    @const name: Name,
    @const uniqId: UniqidOf[ObjectStmtTerm],
    @const extendsClause: Option[Term],
    @const body: Option[BlockTerm],
    @const meta: Option[TermMeta]
) extends TypeDefinition derives ReadWriter {
  override def names: NonEmptyVector[Name] = NonEmptyVector.of(name)
  override type ThisTree = ObjectStmtTerm
  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))

  override def toDoc(using PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    Doc.text("object") <+> Doc.text(name) <+> extendsDoc <+> bodyDoc
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(extendsClause = extendsClause.map(g), body = body.map(g))
  )
}
sealed abstract class TypeDefinition extends StmtTerm with TermWithUniqid derives ReadWriter {
  override type ThisTree <: TypeDefinition
  def names: NonEmptyVector[Name]

  def uniqId: UniqidOf[TypeDefinition]

  override def switchUniqId(r: UReplacer): TypeDefinition
}
case class DotCallTerm(
    @child var record: Term,
    @const fieldName: Name,
    @const args: Vector[Calling] = Vector.empty,
    @child var fieldType: Term,
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  override type ThisTree = DotCallTerm
  override def toDoc(using PrettierOptions): Doc = {
    val argsDoc = if (args.isEmpty) {
      Doc.empty
    } else {
      args.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    }
    group(record.toDoc <> Docs.`.` <> fieldName.toDoc <> argsDoc)
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
    copy(record = f(record), args = args.map(g), fieldType = f(fieldType))
  )
}
