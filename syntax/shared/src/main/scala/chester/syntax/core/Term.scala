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
import com.oracle.truffle.api.frame.VirtualFrame
import spire.math.*
import com.eed3si9n.ifdef.*

import scala.language.implicitConversions
import scala.collection.immutable.HashMap
import scala.collection.immutable.ArraySeq

case class TermMeta(span: Span) extends SpanRequired derives ReadWriter

type ExecuteGeneric = (VirtualFrame, Term) => Object
val globalExecuteGeneric: Parameter[ExecuteGeneric] = new Parameter[ExecuteGeneric]

sealed abstract class Term extends com.oracle.truffle.api.nodes.Node with ToDoc with SpanOptional0 with ContainsUniqid with Tree[Term]
    derives ReadWriter {
  // ThisTree is defined for almost all subclasses of Term, except for MetaTerm
  type ThisTree <: Term
  final def executeGeneric(frame: VirtualFrame): Object = globalExecuteGeneric.get(frame, this)

  def meta: Option[TermMeta]

  def whnf: Trilean

  def span0: Option[Span] = meta.map(_.span)

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
    descentRec {
      case x: TermWithUniqid if x.uniqId == from.uniqId => to
      case x                                            => x
    }
  }

  def collectMeta: Vector[MetaTerm[?]] = {
    this match {
      case term: MetaTerm[?] => return Vector(term)
      case _                 =>
    }
    var result = Vector.empty[MetaTerm[?]]
    inspect(x => result ++= x.collectMeta)
    result
  }

  def replaceMeta(f: MetaTerm[?] => Term): Term = thisOr {
    this match {
      case term: MetaTerm[?] => f(term)
      case _ =>
        descent2([T <: Term] => (x: T) => x.replaceMeta(f).asInstanceOf[x.ThisTree])
    }
  }

  override final def collectU(collector: UCollector): Unit = inspectRec {
    case x: TermWithUniqid => collector(x.uniqId)
    case _                 =>
  }

  override final def replaceU(reranger: UReplacer): Term = descentRec {
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

  override def descent(f: Term => Term, g: TreeMap[Term]): CallingArgTerm = thisOr(
    copy(value = f(value), ty = f(ty))
  )

  override def toDoc(using DocConf): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val nameDoc = name.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    group(nameDoc <+> value.toDoc <+> varargDoc <+> Docs.`:` <+> ty.toDoc)
  }

}

implicit val SeqCallingArgTermRW: ReadWriter[Seq[CallingArgTerm]] =
  readwriter[Seq[Term]].asInstanceOf[ReadWriter[Seq[CallingArgTerm]]]

implicit inline def makeArray[T: scala.reflect.ClassTag](xs: Seq[T]): Array[T] = xs.toArray

open case class Calling(
    args: Seq[CallingArgTerm],
    @const implicitly: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {

  override def toDoc(using DocConf): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`(` <> argsDoc <> Docs.`)` else argsDoc
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Calling = thisOr(
    copy(args = args.map(g(_)))
  )

  override type ThisTree = Calling

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[Calling1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()
  @ifdef("syntax-truffle")
  def copy(
      args: Seq[CallingArgTerm] = this.args,
      implicitly: Boolean = this.implicitly,
      meta: Option[TermMeta] = this.meta
  ): Calling = Calling(args, implicitly, meta)
}
@ifdef("syntax-truffle")
object Calling {
  def apply(args: Seq[CallingArgTerm], implicitly: Boolean = false, meta: Option[TermMeta]): Calling = {
    val args0 = args.toArray
    new Calling1(args0, implicitly, meta)
  }
}
@ifdef("syntax-truffle")
implicit def isCalling1(x: Calling): Calling1 = x.asInstanceOf[Calling1]
@ifdef("syntax-truffle")
class Calling1(
    @children args0: Array[CallingArgTerm],
    @const implicitly: Boolean,
    @const meta: Option[TermMeta]
) extends Calling(ArraySeq.unsafeWrapArray(args0), implicitly, meta) {}

open case class FCallTerm(
    @child var f: Term,
    args: Seq[Calling],
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = FCallTerm

  override def descent(a: Term => Term, g: TreeMap[Term]): FCallTerm = thisOr(
    copy(f = a(f), args = args.map(g(_)))
  )
  override def toDoc(using DocConf): Doc = {
    val fDoc = f.toDoc
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    group(fDoc <+> argsDoc)
  }

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[FCallTerm1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()

  @ifdef("syntax-truffle")
  def copy(
      f: Term = this.f,
      args: Seq[Calling] = this.args,
      meta: Option[TermMeta] = this.meta
  ): FCallTerm = FCallTerm(f, args, meta)
}
@ifdef("syntax-truffle")
object FCallTerm {
  def apply(
      f: Term,
      args: Seq[Calling],
      meta: Option[TermMeta]
  ): FCallTerm1 = {
    val args0 = args.toArray
    new FCallTerm1(f, args0, meta)
  }
}
@ifdef("syntax-truffle")
implicit inline def isFCallTerm1(x: FCallTerm): FCallTerm1 = x.asInstanceOf[FCallTerm1]
@ifdef("syntax-truffle")
class FCallTerm1(
    @child var f0: Term,
    @children args0: Array[Calling],
    @const meta: Option[TermMeta]
) extends FCallTerm(f0, ArraySeq.unsafeWrapArray(args0), meta)
sealed abstract class Pat extends SpecialTerm derives ReadWriter {
  override type ThisTree <: Pat
}
case class Bind(
    @child var bind: LocalV,
    @child var ty: Term,
    @const meta: Option[TermMeta]
) extends Pat derives ReadWriter {
  override type ThisTree = Bind
  override def toDoc(using DocConf): Doc = bind.toDoc <+> Docs.`:` <+> ty.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): Bind = thisOr(copy(bind = g(bind), ty = f(ty)))
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
  // commented out because of MetaTerm
  // override type ThisTree <: SpecialTerm
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

implicit val EffectsMrw: ReadWriter[EffectsM] = union2RW[Effects, MetaTerm[Effects]]
type EffectsM = Effects | MetaTerm[Effects]
case class MetaTerm[T <: Term](@const impl: HoldNotReadable[?], @const meta: Option[TermMeta]) extends SpecialTerm derives ReadWriter {
  override type ThisTree = MetaTerm[T] | T
  override def toDoc(using DocConf): Doc =
    Doc.group("Meta(" <> Doc.text(impl.toString) <> ")")

  inline def unsafeRead[A]: A = impl.inner.asInstanceOf[A]

  override def descent(f: Term => Term, g: TreeMap[Term]): MetaTerm[T] = this
}
extension (g: TreeMap[Term]) {
  // workaround a type checking difficulty
  inline def use(effectsm: EffectsM): EffectsM = g(effectsm).asInstanceOf[EffectsM]
}
case class ListTerm(terms: Seq[Term], @const meta: Option[TermMeta]) extends WHNF derives ReadWriter {
  override type ThisTree = ListTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): ListTerm = thisOr(
    copy(terms = terms.map(f))
  )
  override def toDoc(using DocConf): Doc =
    Doc.mkList(terms, Docs.`[`, Docs.`]`, Doc.empty)

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[ListTerm1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()
  @ifdef("syntax-truffle")
  def copy(terms: Seq[Term] = this.terms, meta: Option[TermMeta] = this.meta): ListTerm = ListTerm(terms, meta)
}
@ifdef("syntax-truffle")
object ListTerm {
  def apply(terms: Seq[Term], meta: Option[TermMeta]): ListTerm = {
    val terms0 = terms.toArray
    new ListTerm1(terms0, meta)
  }
}
@ifdef("syntax-truffle")
implicit def isListTerm1(x: ListTerm): ListTerm1 = x.asInstanceOf[ListTerm1]
@ifdef("syntax-truffle")
class ListTerm1(
    @children terms0: Array[Term],
    @const meta: Option[TermMeta]
) extends ListTerm(ArraySeq.unsafeWrapArray(terms0), meta)
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

  override def descent(f: Term => Term, g: TreeMap[Term]): Type = thisOr(copy(level = f(level)))
  override def toDoc(using DocConf): Doc =
    Doc.mkList(Vector(level), "Type" <> Docs.`(`, Docs.`)`, Doc.empty)
}
case class LevelType(@const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = LevelType
  override def descent(f: Term => Term, g: TreeMap[Term]): LevelType = this
  override def toDoc(using DocConf): Doc =
    Doc.text("LevelType")
}
sealed abstract class Level extends WHNF derives ReadWriter {
  type ThisTree <: Level
}
case class LevelFinite(@child var n: Term, @const meta: Option[TermMeta]) extends Level derives ReadWriter {
  override type ThisTree = LevelFinite
  override def descent(f: Term => Term, g: TreeMap[Term]): LevelFinite = thisOr(copy(n = f(n)))
  override def toDoc(using DocConf): Doc =
    Doc.text("Level(") <> n.toDoc <> Doc.text(")")
}
case class LevelUnrestricted(@const meta: Option[TermMeta]) extends Level derives ReadWriter {
  override type ThisTree = LevelUnrestricted
  override def descent(f: Term => Term, g: TreeMap[Term]): LevelUnrestricted = this
  override def toDoc(using DocConf): Doc =
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
  override def descent(f: Term => Term, g: TreeMap[Term]): Prop = thisOr(copy(level = f(level)))
  override def toDoc(using DocConf): Doc =
    Doc.mkList(Vector(level), "Prop" <> Docs.`(`, Docs.`)`, Doc.empty)
}
// fibrant types
case class FType(@child var level: Term, @const meta: Option[TermMeta]) extends Sort derives ReadWriter {
  override type ThisTree = FType
  override def descent(f: Term => Term, g: TreeMap[Term]): FType = thisOr(copy(level = f(level)))

  override def toDoc(using DocConf): Doc =
    Doc.mkList(Vector(level), "FType" <> Docs.`(`, Docs.`)`, Doc.empty)
}
sealed abstract class LiteralTerm extends WHNF derives ReadWriter {
  override type ThisTree <: LiteralTerm
}
sealed abstract class AbstractIntTerm extends LiteralTerm derives ReadWriter {
  override type ThisTree <: AbstractIntTerm
}
case class IntTerm(@const value: Long, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = IntTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): IntTerm = this
  override def toDoc(using DocConf): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class UIntTerm(@const value: BigInt, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = UIntTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): UIntTerm = this
  override def toDoc(using DocConf): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class IntegerTerm(@const value: BigInt, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = IntegerTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): IntegerTerm = this
  override def toDoc(using DocConf): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class NaturalTerm(@const value: BigInt, @const meta: Option[TermMeta]) extends AbstractIntTerm derives ReadWriter {
  override type ThisTree = NaturalTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): NaturalTerm = this
  override def toDoc(using DocConf): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class IntegerType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = IntegerType
  override def toDoc(using DocConf): Doc =
    Doc.text("Integer", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): IntegerType = this
}
// int of 32 bits or more
case class IntType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = IntType
  override def toDoc(using DocConf): Doc =
    Doc.text("Int", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): IntType = this
}
// unsigned int of 32 bits or more
case class UIntType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = UIntType
  override def toDoc(using DocConf): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): UIntType = this
}
case class NaturalType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = NaturalType
  override def toDoc(using DocConf): Doc =
    Doc.text("Natural", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): NaturalType = this
}
case class RationalTerm(@const value: Rational, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = RationalTerm
  override def toDoc(using DocConf): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): RationalTerm = this
}
case class BooleanTerm(@const value: Boolean, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = BooleanTerm
  override def descent(f: Term => Term, g: TreeMap[Term]): BooleanTerm = this

  override def toDoc(using DocConf): Doc =
    Doc.text(value.toString, ColorProfile.literalColor)
}
case class BooleanType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = BooleanType
  override def toDoc(using DocConf): Doc =
    Doc.text("Boolean", ColorProfile.typeColor)

  override def descent(f: Term => Term, g: TreeMap[Term]): BooleanType = this
}
case class StringTerm(@const value: String, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = StringTerm
  override def toDoc(using DocConf): Doc =
    Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): StringTerm = this
}
case class SymbolTerm(@const value: String, @const meta: Option[TermMeta]) extends LiteralTerm derives ReadWriter {
  override type ThisTree = SymbolTerm
  override def toDoc(using DocConf): Doc =
    Doc.text("'" + value, ColorProfile.literalColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): SymbolTerm = this
}
case class RationalType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = RationalType
  override def toDoc(using DocConf): Doc =
    Doc.text("Rational", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): RationalType = this
}
// float of 32 bits or more
case class FloatType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = FloatType
  override def toDoc(using DocConf): Doc =
    Doc.text("Float", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): FloatType = this
}
case class StringType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = StringType
  override def toDoc(using DocConf): Doc =
    Doc.text("String", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): StringType = this
}
case class SymbolType(@const meta: Option[TermMeta]) extends SimpleType derives ReadWriter {
  override type ThisTree = SymbolType
  override def toDoc(using DocConf): Doc =
    Doc.text("Symbol", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): SymbolType = this
}
case class AnyType(@child var level: Term, @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = AnyType
  override def toDoc(using DocConf): Doc =
    Doc.text("Any", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): AnyType = this
}

val Level0: LevelFinite = LevelFinite(IntegerTerm(0, meta = None), meta = None)
val Type0: Type = Type(Level0, meta = None)
val AnyType0: AnyType = AnyType(Level0, meta = None)
case class NothingType(@const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = NothingType
  override def toDoc(using DocConf): Doc =
    Doc.text("Nothing", ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): NothingType = this
}
case class LiteralType(
    @child var literal: LiteralTerm,
    @const meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = LiteralType
  override def toDoc(using DocConf): Doc =
    Doc.text(literal.toString, ColorProfile.typeColor)
  override def descent(f: Term => Term, g: TreeMap[Term]): LiteralType = this
}
case class ArgTerm(
    @const bind: Option[LocalV],
    @child var ty: Term,
    @const default: Option[Term] = None,
    @const vararg: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = ArgTerm
  override def toDoc(using DocConf): Doc = {
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    val bindDoc = bind.map(_.toDoc <+> Docs.`:`).getOrElse(Doc.empty)
    val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
    group(bindDoc <+> ty.toDoc <+> varargDoc <+> defaultDoc)
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): ArgTerm = thisOr(
    copy(bind = bind.map(g(_)), ty = f(ty), default = default.map(f))
  )
}
case class TelescopeTerm(args: Seq[ArgTerm], @const implicitly: Boolean = false, @const meta: Option[TermMeta]) extends WHNF derives ReadWriter {
  override type ThisTree = TelescopeTerm
  override def toDoc(using DocConf): Doc = {
    val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
    if (implicitly) Docs.`[` <> argsDoc <> Docs.`]` else Docs.`(` <> argsDoc <> Docs.`)`
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): TelescopeTerm = thisOr(
    copy(args = args.map(g(_)))
  )

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[TelescopeTerm1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()
  @ifdef("syntax-truffle")
  def copy(
      args: Seq[ArgTerm] = this.args,
      implicitly: Boolean = this.implicitly,
      meta: Option[TermMeta] = this.meta
  ): TelescopeTerm = TelescopeTerm(args, implicitly, meta)
}
@ifdef("syntax-truffle")
object TelescopeTerm {
  def apply(args: Seq[ArgTerm], implicitly: Boolean = false, meta: Option[TermMeta]): TelescopeTerm = {
    val args0 = args.toArray
    new TelescopeTerm1(args0, implicitly, meta)
  }
}

@ifdef("syntax-truffle")
implicit def isTelescopeTerm1(x: TelescopeTerm): TelescopeTerm1 = x.asInstanceOf[TelescopeTerm1]
@ifdef("syntax-truffle")
class TelescopeTerm1(
    @children args0: Array[ArgTerm],
    @const implicitly: Boolean,
    @const meta: Option[TermMeta]
) extends TelescopeTerm(ArraySeq.unsafeWrapArray(args0), implicitly, meta)
case class Function(
    @child var ty: FunctionType,
    @child var body: Term,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = Function
  override def toDoc(using DocConf): Doc = {
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
  override def descent(f: Term => Term, g: TreeMap[Term]): Function = thisOr(copy(ty = g(ty), body = f(body)))
}
case class FunctionType(
    @const telescopes: Seq[TelescopeTerm],
    @child var resultTy: Term,
    @const effects: EffectsM = Effects.Empty,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = FunctionType
  override def toDoc(using DocConf): Doc = {
    val telescopeDoc =
      telescopes.map(_.toDoc).reduceLeftOption(_ <+> _).getOrElse(Doc.empty)
    val effectsDoc = if (effects.nonEmpty) {
      Docs.`/` <+> effects.toDoc
    } else {
      Doc.empty
    }
    group(telescopeDoc <+> Docs.`->` <+> resultTy.toDoc <> effectsDoc)
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): FunctionType = thisOr(
    copy(
      telescopes = telescopes.map(g(_)),
      resultTy = f(resultTy),
      effects = g.use(effects)
    )
  )

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[FunctionType1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()
  @ifdef("syntax-truffle")
  def copy(
      telescopes: Seq[TelescopeTerm] = this.telescopes,
      resultTy: Term = this.resultTy,
      effects: EffectsM = this.effects,
      meta: Option[TermMeta] = this.meta
  ): FunctionType = FunctionType(telescopes, resultTy, effects, meta)
}
@ifdef("syntax-truffle")
object FunctionType {
  def apply(
      telescopes: Seq[TelescopeTerm],
      resultTy: Term,
      effects: EffectsM = Effects.Empty,
      meta: Option[TermMeta]
  ): FunctionType = {
    val telescopes0 = telescopes.toArray
    new FunctionType1(telescopes0, resultTy, effects, meta)
  }
}
@ifdef("syntax-truffle")
class FunctionType1(
    @children telescopes0: Array[TelescopeTerm],
    @child var resultTy1: Term,
    @const effects: EffectsM,
    @const meta: Option[TermMeta]
) extends FunctionType(ArraySeq.unsafeWrapArray(telescopes0), resultTy1, effects, meta)
case class ObjectClauseValueTerm(
    @child var key: Term,
    @child var value: Term,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = ObjectClauseValueTerm
  override def toDoc(using DocConf): Doc = group(
    key.toDoc <+> Doc.text("=") <+> value.toDoc
  )

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectClauseValueTerm = thisOr(
    copy(key = f(key), value = f(value))
  )
}

case class ObjectTerm(
    clauses: Seq[ObjectClauseValueTerm],
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = ObjectTerm
  override def toDoc(using DocConf): Doc =
    Doc.mkList(clauses.map(_.toDoc), Docs.`{`, Docs.`}`, Doc.empty)

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectTerm = thisOr(
    copy(clauses = clauses.map(g(_)))
  )

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[ObjectTerm1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()
  @ifdef("syntax-truffle")
  def copy(
      clauses: Seq[ObjectClauseValueTerm] = this.clauses,
      meta: Option[TermMeta] = this.meta
  ): ObjectTerm = ObjectTerm(clauses, meta)
}
@ifdef("syntax-truffle")
object ObjectTerm {
  def apply(clauses: Seq[ObjectClauseValueTerm], meta: Option[TermMeta]): ObjectTerm = {
    val clauses0 = clauses.toArray
    new ObjectTerm1(clauses0, meta)
  }
}
@ifdef("syntax-truffle")
implicit def isObjectTerm1(x: ObjectTerm): ObjectTerm1 = x.asInstanceOf[ObjectTerm1]
@ifdef("syntax-truffle")
class ObjectTerm1(
    @children clauses0: Array[ObjectClauseValueTerm],
    @const meta: Option[TermMeta]
) extends ObjectTerm(ArraySeq.unsafeWrapArray(clauses0), meta)

open case class ObjectType(
    fieldTypes: Seq[ObjectClauseValueTerm],
    @const exactFields: Boolean = false,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = ObjectType
  override def toDoc(using DocConf): Doc =
    Doc.mkList(
      fieldTypes.map(_.toDoc),
      "Object" </> Docs.`{`,
      Docs.`}`,
      Doc.empty
    )
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectType = thisOr(
    copy(fieldTypes = fieldTypes.map(g(_)))
  )
  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[ObjectType1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()

  @ifdef("syntax-truffle")
  def copy(
      fieldTypes: Seq[ObjectClauseValueTerm] = this.fieldTypes,
      exactFields: Boolean = this.exactFields,
      meta: Option[TermMeta] = this.meta
  ): ObjectType = ObjectType(fieldTypes, exactFields, meta)
}
@ifdef("syntax-truffle")
object ObjectType {
  def apply(fieldTypes: Seq[ObjectClauseValueTerm], exactFields: Boolean = false, meta: Option[TermMeta]): ObjectType = {
    val fieldTypes0 = fieldTypes.toArray
    new ObjectType1(fieldTypes0, exactFields, meta)
  }
}
@ifdef("syntax-truffle")
implicit def isObjectType1(x: ObjectType): ObjectType1 = x.asInstanceOf[ObjectType1]
// exactFields is a hint: subtype relationship should not include different number of fields. Otherwise, throw a warning (only warning no error)
@ifdef("syntax-truffle")
class ObjectType1(
    @children fieldTypes0: Array[ObjectClauseValueTerm],
    @const exactFields: Boolean,
    @const meta: Option[TermMeta]
) extends ObjectType(ArraySeq.unsafeWrapArray(fieldTypes0), exactFields, meta)
case class ListF(@const meta: Option[TermMeta]) extends Builtin derives ReadWriter {
  override type ThisTree = ListF
  override def toDoc(using DocConf): Doc = "List"
  override def descent(f: Term => Term, g: TreeMap[Term]): ListF = this
}
sealed abstract class Constructed extends WHNF derives ReadWriter {
  type ThisTree <: Constructed
}
case class ListType(@child var ty: Term, @const meta: Option[TermMeta]) extends Constructed with TypeTerm derives ReadWriter {
  override type ThisTree = ListType
  override def toDoc(using DocConf): Doc =
    Doc.text("List") <> Docs.`(` <> ty <> Docs.`)`
  override def descent(f: Term => Term, g: TreeMap[Term]): ListType = thisOr(copy(ty = f(ty)))
}
case class Union(@const xs: NonEmptyVector[Term], @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = Union
  override def toDoc(using DocConf): Doc =
    Doc.mkList(xs, Docs.`(`, Docs.`)`, "|")

  override def descent(f: Term => Term, g: TreeMap[Term]): Union = thisOr(
    copy(xs = xs.map(f))
  )
}
def Union1(xs: NonEmptyVector[Term], meta: Option[TermMeta]): Term = if (xs.length == 1) {
  xs.head
} else {
  Union(xs, meta)
}
case class Intersection(@const xs: NonEmptyVector[Term], @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = Intersection
  override def toDoc(using DocConf): Doc =
    Doc.mkList(xs, Docs.`(`, Docs.`)`, "&")

  override def descent(f: Term => Term, g: TreeMap[Term]): Intersection = thisOr(copy(xs = xs.map(f)))
}
sealed abstract class Builtin extends WHNF derives ReadWriter {
  override type ThisTree <: Builtin
}
sealed abstract class Effect extends WHNF derives ReadWriter {
  override type ThisTree <: Effect
}
case class Effects(@const effects: Map[LocalV, Term] = HashMap.empty, @const meta: Option[TermMeta]) extends WHNF derives ReadWriter {

  override type ThisTree = Effects
  override def toDoc(using DocConf): Doc =
    Doc.mkList(
      effects.map((k, v) => k.toDoc <+> Docs.`:` <+> v.toDoc),
      Docs.`{`,
      Docs.`}`,
      Doc.empty
    )

  override def collectMeta: Vector[MetaTerm[?]] =
    effects.flatMap((a, b) => a.collectMeta ++ b.collectMeta).toVector

  override def replaceMeta(f: MetaTerm[?] => Term): Term =
    copy(effects = effects.map((a, b) => (a.replaceMeta(f).asInstanceOf[LocalV], b.replaceMeta(f))))
  def isEmpty: Boolean = effects.isEmpty

  def nonEmpty: Boolean = effects.nonEmpty

  override def descent(f: Term => Term, g: TreeMap[Term]): Effects = thisOr(copy(effects = effects.map { case (k, v) => (k, f(v)) }))
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
  override def toDoc(using DocConf): Doc = Doc.text(name)

  override def descent(f: Term => Term, g: TreeMap[Term]): ExceptionEffect = this
}
// todo: may not terminate

// todo:  whatever IO: console, file, network, ...
sealed abstract class ReferenceCall extends Uneval with TermWithUniqid derives ReadWriter {
  override type ThisTree <: ReferenceCall
  override def uniqId: UniqidOf[ReferenceCall]
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
  override def toDoc(using DocConf): Doc = Doc.text(name)
  override def descent(f: Term => Term, g: TreeMap[Term]): LocalV = thisOr(copy(ty = f(ty)))
  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))

}
case class ToplevelV(
    @const id: AbsoluteRef,
    @child var ty: Term,
    @const uniqId: UniqidOf[ToplevelV],
    @const meta: Option[TermMeta]
) extends ReferenceCall derives ReadWriter {
  override type ThisTree = ToplevelV
  override def toDoc(using DocConf): Doc = group(
    id.toDoc <+> Docs.`.` <+> ty.toDoc
  )
  override def descent(f: Term => Term, g: TreeMap[Term]): ToplevelV = thisOr(copy(ty = f(ty)))

  override def switchUniqId(r: UReplacer): ThisTree = copy(uniqId = r(uniqId))
  @deprecated("dont use")
  def name: Name = id.name
}
case class ErrorTerm(@const problem: Problem, @const meta: Option[TermMeta]) extends SpecialTerm derives ReadWriter {
  override type ThisTree = ErrorTerm
  override def toDoc(using DocConf): Doc = problem.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): ErrorTerm = this
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
  override def toDoc(using DocConf): Doc =
    Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): LetStmtTerm = thisOr(
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
  override def toDoc(using DocConf): Doc =
    Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
  override def descent(f: Term => Term, g: TreeMap[Term]): DefStmtTerm = thisOr(
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
  override def toDoc(using DocConf): Doc = expr.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): ExprStmtTerm = thisOr(
    copy(expr = f(expr), ty = f(ty))
  )
}
case class NonlocalOrLocalReturn(@child var value: Term, @const meta: Option[TermMeta]) extends StmtTerm derives ReadWriter {
  override type ThisTree = NonlocalOrLocalReturn
  override def toDoc(using DocConf): Doc =
    Doc.text("return") <+> value.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): NonlocalOrLocalReturn = thisOr(
    copy(value = f(value))
  )
}
case class TupleType(@const types: Vector[Term], @const meta: Option[TermMeta]) extends TypeTerm derives ReadWriter {
  override type ThisTree = TupleType
  override def toDoc(using DocConf): Doc =
    Doc.mkList(types, "Tuple" <> Docs.`[`, Docs.`]`, Doc.empty)
  override def descent(f: Term => Term, g: TreeMap[Term]): TupleType = thisOr(
    copy(types = types.map(f))
  )
}
case class TupleTerm(@const values: Vector[Term], @const meta: Option[TermMeta]) extends WHNF derives ReadWriter {
  override type ThisTree = TupleTerm
  override def toDoc(using DocConf): Doc =
    Doc.mkList(values, Docs.`(`, Docs.`)`, Doc.empty)
  override def descent(f: Term => Term, g: TreeMap[Term]): TupleTerm = thisOr(
    copy(values = values.map(f))
  )
}
case class BlockTerm(
    statements: Seq[StmtTerm],
    @child var result: Term,
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  override type ThisTree = BlockTerm
  override def toDoc(using DocConf): Doc =
    Doc.mkList(statements.map(_.toDoc) :+ result.toDoc, Docs.`{`, Docs.`}`, ";")

  override def descent(f: Term => Term, g: TreeMap[Term]): BlockTerm = thisOr(
    copy(statements = statements.map(g(_)), result = f(result))
  )

  @ifdef("syntax-truffle")
  private inline def validate(): Unit = require(this.isInstanceOf[BlockTerm1], "please call apply instead of new")
  @ifndef("syntax-truffle")
  private inline def validate(): Unit = ()
  validate()
  @ifdef("syntax-truffle")
  def copy(
      statements: Seq[StmtTerm] = this.statements,
      result: Term = this.result,
      meta: Option[TermMeta] = this.meta
  ): BlockTerm = BlockTerm(statements, result, meta)
}
@ifdef("syntax-truffle")
object BlockTerm {
  def apply(statements: Seq[StmtTerm], result: Term, meta: Option[TermMeta]): BlockTerm = {
    val statements0 = statements.toArray
    new BlockTerm1(statements0, result, meta)
  }
}
@ifdef("syntax-truffle")
implicit def isBlockTerm1(x: BlockTerm): BlockTerm1 = x.asInstanceOf[BlockTerm1]
@ifdef("syntax-truffle")
class BlockTerm1(
    @children statements0: Array[StmtTerm],
    @child var result1: Term,
    @const meta: Option[TermMeta]
) extends BlockTerm(ArraySeq.unsafeWrapArray(statements0), result1, meta)
case class Annotation(
    @child var term: Term,
    @const ty: Option[Term],
    @const effects: Option[EffectsM],
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  require(ty.nonEmpty || effects.nonEmpty)
  override type ThisTree = Annotation
  override def toDoc(using DocConf): Doc = {
    val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
    val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
    term.toDoc <> tyDoc <> effectsDoc
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): Annotation = thisOr(
    copy(
      term = f(term),
      ty = ty.map(f),
      effects = effects.map(g(_)).asInstanceOf[Option[EffectsM]]
    )
  )
}
case class FieldTerm(
    @const name: Name,
    @child var ty: Term,
    @const meta: Option[TermMeta]
) extends WHNF derives ReadWriter {
  override type ThisTree = FieldTerm
  override def toDoc(using DocConf): Doc =
    Doc.text(name) <> Doc.text(": ") <> ty.toDoc

  override def descent(f: Term => Term, g: TreeMap[Term]): FieldTerm = thisOr(
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

  override def toDoc(using DocConf): Doc = {
    val fieldsDoc = fields.map(_.toDoc).reduceOption(_ <> Doc.text(", ") <> _).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).reduceOption(_ </> _).getOrElse(Doc.empty)

    group(
      Doc.text("record ") <> Doc.text(name) <>
        Doc.group(Doc.text("(") <> fieldsDoc <> Doc.text(")")) <>
        bodyDoc
    )
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): RecordStmtTerm = thisOr(
    copy(
      fields = fields.map(g(_)),
      body = body.map(g(_)),
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
  override def toDoc(using DocConf): Doc = {
    val argsDoc = Doc.mkList(args.map(_.toDoc), Docs.`(`, Docs.`)`, Docs.`,`)
    Doc.text(recordName) <> argsDoc
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): RecordConstructTerm = thisOr(
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

  override def toDoc(using DocConf): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("trait ") <> Doc.text(name) <> extendsDoc <> bodyDoc
    )
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): TraitStmtTerm = thisOr(
    copy(extendsClause = extendsClause.map(g(_)), body = body.map(g(_)))
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

  override def toDoc(using DocConf): Doc = {
    val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("interface ") <> Doc.text(name) <> extendsDoc <> bodyDoc
    )
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): InterfaceStmtTerm = thisOr(
    copy(extendsClause = extendsClause.map(g(_)), body = body.map(g(_)))
  )
}
case class RecordTypeTerm(
    recordDef: RecordStmtTerm,
    telescope: TelescopeTerm,
    meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = RecordTypeTerm
  override def toDoc(using DocConf): Doc =
    group("RecordCall" <+> recordDef.toDoc <> telescope.toDoc)

  override def descent(f: Term => Term, g: TreeMap[Term]): RecordTypeTerm = thisOr(
    copy(recordDef = g(recordDef), telescope = g(telescope))
  )
}
case class TraitTypeTerm(
    @child var traitDef: TraitStmtTerm,
    @const meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = TraitTypeTerm
  override def toDoc(using DocConf): Doc =
    Doc.text("TraitCall(") <> traitDef.name.toDoc <> Doc.text(")")

  override def descent(f: Term => Term, g: TreeMap[Term]): TraitTypeTerm = thisOr(
    copy(traitDef = g(traitDef))
  )
}
case class ObjectConstructTerm(
    @child var objectRef: Term,
    @const meta: Option[TermMeta]
) extends Uneval derives ReadWriter {
  override type ThisTree = ObjectConstructTerm
  override def toDoc(using DocConf): Doc =
    group("ObjectCall" <+> objectRef.toDoc)

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectConstructTerm = thisOr(
    copy(objectRef = f(objectRef))
  )
}
case class ObjectTypeTerm(
    @child var objectDef: ObjectStmtTerm,
    @const meta: Option[TermMeta]
) extends TypeTerm derives ReadWriter {
  override type ThisTree = ObjectTypeTerm
  override def toDoc(using DocConf): Doc =
    Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectTypeTerm = thisOr(
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

  override def toDoc(using DocConf): Doc = {
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    Doc.text("object") <+> Doc.text(name) <+> extendsDoc <+> bodyDoc
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): ObjectStmtTerm = thisOr(
    copy(extendsClause = extendsClause.map(g(_)), body = body.map(g(_)))
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
  override def toDoc(using DocConf): Doc = {
    val argsDoc = if (args.isEmpty) {
      Doc.empty
    } else {
      args.map(_.toDoc).reduceOption(_ <+> _).getOrElse(Doc.empty)
    }
    group(record.toDoc <> Docs.`.` <> fieldName.toDoc <> argsDoc)
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): DotCallTerm = thisOr(
    copy(record = f(record), args = args.map(g(_)), fieldType = f(fieldType))
  )
}
case class HoleTerm(
    @const name: Name,
    @const ty: Term,
    @const effects: Option[EffectsM] = None,
    @const meta: Option[TermMeta]
) extends SpecialTerm derives ReadWriter {
  override type ThisTree = HoleTerm
  override def toDoc(using DocConf): Doc = {
    val effectsDoc = effects.map(e => Docs.`/` <+> e.toDoc).getOrElse(Doc.empty)
    Doc.text("Hole") <> Doc.text(name) <> Docs.`:` <+> ty.toDoc <> effectsDoc
  }

  override def descent(f: Term => Term, g: TreeMap[Term]): HoleTerm = thisOr(
    copy(ty = f(ty), effects = effects.map(g.use))
  )
}

case class NativeTerm(
    @const repr: Term,
    @const ty: Term,
    @const effects: Option[EffectsM] = None,
    @const meta: Option[TermMeta]
) extends SpecialTerm derives ReadWriter {
  override type ThisTree = NativeTerm
  override def toDoc(using DocConf): Doc = {
    val effectsDoc = effects.map(e => Docs.`/` <+> e.toDoc).getOrElse(Doc.empty)
    Doc.text("Native") <> repr.toDoc <> Docs.`:` <+> ty.toDoc <> effectsDoc
  }
  override def descent(f: Term => Term, g: TreeMap[Term]): NativeTerm = thisOr(
    copy(
      repr = f(repr),
      ty = f(ty),
      effects = effects.map(g.use)
    )
  )
}
