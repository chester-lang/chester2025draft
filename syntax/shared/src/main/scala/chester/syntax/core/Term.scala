// TODO: More correctly implement toDoc
package chester.syntax.core.spec

import cats.data.*
import chester.doc.*
import chester.doc.const.{ColorProfile, Docs}
import chester.error.*
import chester.syntax.*
import chester.syntax.core.orm.*
import chester.syntax.core.simple.{ObjectStmtTerm, Term, TermWithUniqid}
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
 *     - LocalVC[Term] - Concrete implementation
 *   - LocalVF[Term, ThisTree] - Factory for creating LocalVC[Term] instances
 */
object spec {

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

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    def cons: FCallTermF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc = {
      val fDoc = f.toDoc
      val argsDoc = args.map(_.toDoc).reduce(_ <+> _)
      group(fDoc <+> argsDoc)
    }

    def cpy(f: Term = f, args: Vector[CallingC[Term]] = args, meta: OptionTermMeta = meta): ThisTree =
      cons.newFCallTerm(f, args, meta)

     override def descent(a: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc = bind.toDoc <+> Docs.`:` <+> ty.toDoc

    def cpy(bind: LocalVC[Term] = bind, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree = cons.newBind(bind, ty, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(bind = g(bind), ty = f(ty)))
  }

  /** more abstract Term. sealed trait *T corresponds to sealed trait in Term; trait *C corresponds to case class in Term */
  trait TermT[Term <: TermT[Term]] extends Any with ToDoc with ContainsUniqid with Tree[Term] {
    override type ThisTree <: TermT[Term]
    override def toDoc(using options: PrettierOptions): Doc = toString

     override def descent(f: Term => Term, g: TreeMap[Term]): Term

    def mapFlatten[B](f: Term => Seq[B]): Vector[B] = {
      var result = Vector.empty[B]
      inspectRecursive { term =>
        result ++= f(term)
      }
      result
    }

    def meta: OptionTermMeta

    def whnf: Trilean

    def sourcePos: Option[SourcePos] = meta.map(_.sourcePos)

    def doElevate(level: IntegerTermC[Term]): Term = descent(_.doElevate(level))

    final def elevate(level: IntegerTermC[Term]): Term = {
      require(level.value >= 0)
      if (level.value == 0) this else doElevate(level)
    }

    // TODO: optimize
    final def substitute[A <: TermWithUniqidT[Term]](mapping: Seq[(A, Term)]): Term = {
      mapping.foldLeft(this) { case (acc, (from, to)) =>
        acc.substitute(from, to)
      }
    }

    final def substitute(from: TermWithUniqidT[Term], to: Term): Term = {
      if (from == to) return this
      if (
        to match {
          case to: TermWithUniqidT[Term] => from.uniqId == to.uniqId
          case _                         => false
        }
      ) return this
      descentRecursive {
        case x: TermWithUniqidT[Term] if x.uniqId == from.uniqId => to
        case x                                                   => x
      }
    }

    def collectMeta: Vector[MetaTermC[Term]] = {
      this match {
        case term: MetaTermC[Term] => return Vector(term)
        case _              =>
      }
      var result = Vector.empty[MetaTermC[Term]]
      inspect { x => result ++= x.collectMeta }
      result
    }

    def replaceMeta(f: MetaTermC[Term] => Term): Term = thisOr {
      this match {
        case term: MetaTermC[Term] => f(term)
        case _ =>
          descent2(new TreeMap[Term] {
            def use[T <: Term](x: T): x.ThisTree = x.replaceMeta(f).asInstanceOf[x.ThisTree]
          })
      }
    }

    final override def collectU(collector: UCollector): Unit = inspectRecursive {
      case x: TermWithUniqidT[Term] => collector(x.uniqId)
      case _                        =>
    }

    final override def replaceU(reranger: UReplacer): Term = descentRecursive {
      case x: TermWithUniqidT[Term] => x.switchUniqId(reranger)
      case x                        => x
    }
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

    def switchUniqId(r: UReplacer): TermWithUniqidT[Term]
  }

  trait EffectsMT[Term <: TermT[Term]] extends TermT[Term] {
    override type ThisTree <: EffectsMT[Term]
  }

  trait MetaTermC[Term <: TermT[Term]] extends TermT[Term] with EffectsMT[Term] with SpecialTermT[Term] {
    override type ThisTree <: MetaTermC[Term]

    def impl: HoldNotReadable[?]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.group("Meta" <> Doc.text(impl.toString))

    def unsafeRead[T]: T = impl.inner.asInstanceOf[T]

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  implicit def metaVec[Term <: TermT[Term], ThisTree <: MetaTermC[Term]](x: Vector[MetaTermC[Term]]): Vector[ThisTree] = x.asInstanceOf[Vector[ThisTree]]

  @FunctionalInterface
  trait ListTermF[Term <: TermT[Term], ThisTree <: ListTermC[Term]] {
    def newListTerm(terms: Vector[Term], meta: OptionTermMeta): ThisTree
  }

  trait ListTermC[Term <: TermT[Term]] extends TermT[Term] with WHNFT[Term] {
    override type ThisTree <: ListTermC[Term]

    def terms: Vector[Term]

    def cons: ListTermF[Term, ThisTree]

    def cpy(terms: Vector[Term] = this.terms, meta: OptionTermMeta = this.meta): ThisTree =
      cons.newListTerm(terms, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(terms = terms.map(f))
    )
  }

  trait TypeTermT[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: TypeTermT[Term]
  }

  trait SortT[Term <: TermT[Term]] extends TypeTermT[Term] {
    override type ThisTree <: SortT[Term]

    def level: TermT[Term]
  }

  @FunctionalInterface
  trait TypeF[Term <: TermT[Term], ThisTree <: TypeC[Term]] {
    def newType(level: Term, meta: OptionTermMeta): ThisTree
  }

  trait TypeC[Term <: TermT[Term]] extends SortT[Term] {
    override type ThisTree <: TypeC[Term]

    def level: Term

    def cons: TypeF[Term, ThisTree]

    def cpy(level: Term = level, meta: OptionTermMeta = meta): ThisTree = cons.newType(level, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(level = f(level)))
  }
  @FunctionalInterface
  trait LevelTypeF[Term <: TermT[Term], ThisTree <: LevelTypeC[Term]] {
    def newLevelType(meta: OptionTermMeta): ThisTree
  }

  trait LevelTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: LevelTypeC[Term]

    def cons: LevelTypeF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newLevelType(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  trait LevelT[Term <: TermT[Term]] extends TypeTermT[Term] with WHNFT[Term] {
    override type ThisTree <: LevelT[Term]
  }

  @FunctionalInterface
  trait LevelFiniteF[Term <: TermT[Term], ThisTree <: LevelFiniteC[Term]] {
    def newLevelFinite(n: Term, meta: OptionTermMeta): ThisTree
  }

  trait LevelFiniteC[Term <: TermT[Term]] extends LevelT[Term] {
    override type ThisTree <: LevelFiniteC[Term]

    def n: Term

    def cons: LevelFiniteF[Term, ThisTree]

    def cpy(n: Term = n, meta: OptionTermMeta = meta): ThisTree = cons.newLevelFinite(n, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(n = f(n)))
  }

  @FunctionalInterface
  trait LevelUnrestrictedF[Term <: TermT[Term], ThisTree <: LevelUnrestrictedC[Term]] {
    def newLevelUnrestricted(meta: OptionTermMeta): ThisTree
  }

  trait LevelUnrestrictedC[Term <: TermT[Term]] extends LevelT[Term] {
    override type ThisTree <: LevelUnrestrictedC[Term]

    def cons: LevelUnrestrictedF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newLevelUnrestricted(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  enum Usage derives ReadWriter {
    case None, Linear, Unrestricted
  }

  @FunctionalInterface
  trait PropF[Term <: TermT[Term], ThisTree <: PropC[Term]] {
    def newProp(level: Term, meta: OptionTermMeta): ThisTree
  }

  trait PropC[Term <: TermT[Term]] extends SortT[Term] {
    override type ThisTree <: PropC[Term]

    def level: Term

    def cons: PropF[Term, ThisTree]

    def cpy(level: Term = level, meta: OptionTermMeta = meta): ThisTree = cons.newProp(level, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(level = f(level)))
  }

  @FunctionalInterface
  trait FTypeF[Term <: TermT[Term], ThisTree <: FTypeC[Term]] {
    def newFType(level: Term, meta: OptionTermMeta): ThisTree
  }

  trait FTypeC[Term <: TermT[Term]] extends SortT[Term] {
    override type ThisTree <: FTypeC[Term]

    def level: Term

    def cons: FTypeF[Term, ThisTree]

    def cpy(level: Term = level, meta: OptionTermMeta = meta): ThisTree = cons.newFType(level, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(level = f(level)))

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist("FType" <> Docs.`(`, Docs.`)`)(Vector(level))
  }

  trait LiteralTermT[Term <: TermT[Term]] extends TermT[Term] with WHNFT[Term] {
    override type ThisTree <: LiteralTermT[Term]
  }

  trait AbstractIntTermT[Term <: TermT[Term]] extends LiteralTermT[Term] {
    override type ThisTree <: AbstractIntTermT[Term]
  }

  @FunctionalInterface
  trait IntTermF[Term <: TermT[Term], ThisTree <: IntTermC[Term]] {
    def newIntTerm(value: Int, meta: OptionTermMeta): ThisTree
  }

  trait IntTermC[Term <: TermT[Term]] extends LiteralTermT[Term] with AbstractIntTermT[Term] {
    override type ThisTree <: IntTermC[Term]

    def value: Int
    def cons: IntTermF[Term, ThisTree]
    def cpy(value: Int = value, meta: OptionTermMeta = meta): ThisTree = cons.newIntTerm(value, meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait IntegerTermF[Term <: TermT[Term], ThisTree <: IntegerTermC[Term]] {
    def newIntegerTerm(value: BigInt, meta: OptionTermMeta): ThisTree
  }
  
  object IntegerTermF {
    def apply[Term <: TermT[Term],ThisTree <: IntegerTermC[Term]](value: BigInt, meta: OptionTermMeta)(using f:IntegerTermF[Term, ThisTree]): ThisTree =
      f.newIntegerTerm(value, meta)
      
  }

  trait IntegerTermC[Term <: TermT[Term]] extends LiteralTermT[Term] with AbstractIntTermT[Term] {
    override type ThisTree <: IntegerTermC[Term]

    def value: BigInt

    def cons: IntegerTermF[Term, ThisTree]

    def cpy(value: BigInt = value, meta: OptionTermMeta = meta): ThisTree = cons.newIntegerTerm(value, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait IntegerTypeF[Term <: TermT[Term], ThisTree <: IntegerTypeC[Term]] {
    def newIntegerType(meta: OptionTermMeta): ThisTree
  }

  trait IntegerTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: IntegerTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Integer", ColorProfile.typeColor)

    def cons: IntegerTypeF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newIntegerType(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait IntTypeF[Term <: TermT[Term], ThisTree <: IntTypeC[Term]] {
    def newIntType(meta: OptionTermMeta): ThisTree
  }

  trait IntTypeC[Term <: TermT[Term]] extends TypeTermT[Term]  {
    override type ThisTree <: IntTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Int", ColorProfile.typeColor)

    def cons: IntTypeF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newIntType(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }


@FunctionalInterface
  trait UIntTypeF[Term <: TermT[Term], ThisTree <: UIntTypeC[Term]] {
    def newUIntType(meta: OptionTermMeta): ThisTree
  }

  trait UIntTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: UIntTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("UInt", ColorProfile.typeColor)

    def cons: UIntTypeF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newUIntType(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait NaturalTypeF[Term <: TermT[Term], ThisTree <: NaturalTypeC[Term]] {
    def newNaturalType(meta: OptionTermMeta): ThisTree
  }

  trait NaturalTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: NaturalTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Natural", ColorProfile.typeColor)

    def cons: NaturalTypeF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newNaturalType(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait RationalTermF[Term <: TermT[Term], ThisTree <: RationalTermC[Term]] {
    def newRationalTerm(value: Rational, meta: OptionTermMeta): ThisTree
  }

  trait RationalTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
    override type ThisTree <: RationalTermC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text(value.toString, ColorProfile.literalColor)
    def value: Rational
    def cons: RationalTermF[Term, ThisTree]
    def cpy(value: Rational = value, meta: OptionTermMeta = meta): ThisTree = cons.newRationalTerm(value, meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait BooleanTermF[Term <: TermT[Term], ThisTree <: BooleanTermC[Term]] {
    def newBooleanTerm(value: Boolean, meta: OptionTermMeta): ThisTree
  }

  trait BooleanTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
    override type ThisTree <: BooleanTermC[Term]

    def value: Boolean

    def cons: BooleanTermF[Term, ThisTree]

    def cpy(value: Boolean = value, meta: OptionTermMeta = meta): ThisTree = cons.newBooleanTerm(value, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text(value.toString, ColorProfile.literalColor)
  }

  @FunctionalInterface
  trait BooleanTypeF[Term <: TermT[Term], ThisTree <: BooleanTypeC[Term]] {
    def newBooleanType(meta: OptionTermMeta): ThisTree
  }

  trait BooleanTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: BooleanTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Boolean", ColorProfile.typeColor)

    def cons: BooleanTypeF[Term, ThisTree]

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newBooleanType(meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait StringTermF[Term <: TermT[Term], ThisTree <: StringTermC[Term]] {
    def newStringTerm(value: String, meta: OptionTermMeta): ThisTree
  }

  trait StringTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
    override type ThisTree <: StringTermC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("\"" + encodeString(value) + "\"", ColorProfile.literalColor)
    def value: String
    def cons: StringTermF[Term, ThisTree]
    def cpy(value: String = value, meta: OptionTermMeta = meta): ThisTree = cons.newStringTerm(value, meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait SymbolTermF[Term <: TermT[Term], ThisTree <: SymbolTermC[Term]] {
    def newSymbolTerm(value: String, meta: OptionTermMeta): ThisTree
  }

  trait SymbolTermC[Term <: TermT[Term]] extends LiteralTermT[Term] {
    override type ThisTree <: SymbolTermC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("'" + value, ColorProfile.literalColor)
    def value: String
    def cons: SymbolTermF[Term, ThisTree]
    def cpy(value: String = value, meta: OptionTermMeta = meta): ThisTree = cons.newSymbolTerm(value, meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait RationalTypeF[Term <: TermT[Term], ThisTree <: RationalTypeC[Term]] {
    def newRationalType(meta: OptionTermMeta): ThisTree
  }

  trait RationalTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: RationalTypeC[Term]
    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Rational", ColorProfile.typeColor)
    def cons: RationalTypeF[Term, ThisTree]
    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newRationalType(meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait FloatTypeF[Term <: TermT[Term], ThisTree <: FloatTypeC[Term]] {
    def newFloatType(meta: OptionTermMeta): ThisTree
  }

  trait FloatTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: FloatTypeC[Term]
    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Float", ColorProfile.typeColor)
    def cons: FloatTypeF[Term, ThisTree]
    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newFloatType(meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait StringTypeF[Term <: TermT[Term], ThisTree <: StringTypeC[Term]] {
    def newStringType(meta: OptionTermMeta): ThisTree
  }

  trait StringTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: StringTypeC[Term]
    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("String", ColorProfile.typeColor)
    def cons: StringTypeF[Term, ThisTree]
    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newStringType(meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait SymbolTypeF[Term <: TermT[Term], ThisTree <: SymbolTypeC[Term]] {
    def newSymbolType(meta: OptionTermMeta): ThisTree
  } 

  trait SymbolTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: SymbolTypeC[Term]
    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Symbol", ColorProfile.typeColor)
    def cons: SymbolTypeF[Term, ThisTree]
    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newSymbolType(meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait AnyTypeF[Term <: TermT[Term], ThisTree <: AnyTypeC[Term]] {
    def newAnyType(level: Term, meta: OptionTermMeta): ThisTree
  }

  trait AnyTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: AnyTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Any", ColorProfile.typeColor)
    def level: Term
    def cons: AnyTypeF[Term, ThisTree]
    def cpy(level: Term = level, meta: OptionTermMeta = meta): ThisTree = cons.newAnyType(level, meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }
  
  def Level0[Term <: TermT[Term], LevelFinite <: LevelFiniteC[Term], IntegerTerm <: IntegerTermC[Term]]
  (using aLevelFiniteF: LevelFiniteF[Term, LevelFinite], aIntegerTermF: IntegerTermF[Term, IntegerTerm]): LevelFinite = aLevelFiniteF.newLevelFinite(aIntegerTermF.newIntegerTerm(0, meta = None), meta = None)

  def Type0[Term <: TermT[Term], Type <: TypeC[Term], LevelFinite <: LevelFiniteC[Term], IntegerTerm <: IntegerTermC[Term]]
  (using aTypeF: TypeF[Term, Type], x0: LevelFiniteF[Term, LevelFinite], x1: IntegerTermF[Term, IntegerTerm]): Type = aTypeF.newType(Level0[Term, LevelFinite, IntegerTerm](using x0, x1), meta = None)

  def AnyType0[Term <: TermT[Term], AnyType <: AnyTypeC[Term], LevelFinite <: LevelFiniteC[Term], IntegerTerm <: IntegerTermC[Term]]
  (using aAnyTypeF: AnyTypeF[Term, AnyType], x0: LevelFiniteF[Term, LevelFinite], x1:IntegerTermF[Term, IntegerTerm]): AnyType = aAnyTypeF.newAnyType(Level0[Term, LevelFinite, IntegerTerm](using x0, x1), meta = None)

  @FunctionalInterface
  trait NothingTypeF[Term <: TermT[Term], ThisTree <: NothingTypeC[Term]] {
    def newNothingType(meta: OptionTermMeta): ThisTree
  }

  trait NothingTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: NothingTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("Nothing", ColorProfile.typeColor)
    def cons: NothingTypeF[Term, ThisTree]
    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newNothingType(meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait LiteralTypeF[Term <: TermT[Term], ThisTree <: LiteralTypeC[Term]] {
    def newLiteralType(literal: LiteralTermT[Term], meta: OptionTermMeta): ThisTree
  }

  trait LiteralTypeC[Term <: TermT[Term]] extends TypeTermT[Term]   {
    override type ThisTree <: LiteralTypeC[Term]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text(literal.toString, ColorProfile.typeColor)
    def literal: LiteralTermT[Term]
    def cons: LiteralTypeF[Term, ThisTree]
    def cpy(literal: LiteralTermT[Term] = literal, meta: OptionTermMeta = meta): ThisTree = cons.newLiteralType(literal, meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  @FunctionalInterface
  trait ArgTermF[Term <: TermT[Term], ThisTree <: ArgTermC[Term]] {
    def newArgTerm(bind: LocalVC[Term], ty: Term, default: Option[Term], vararg: Boolean, meta: OptionTermMeta): ThisTree
  }

  trait ArgTermC[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: ArgTermC[Term]

    def bind: LocalVC[Term]

    def ty: Term

    def default: Option[Term]

    def vararg: Boolean

    def cons: ArgTermF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc = {
      val varargDoc = if (vararg) Docs.`...` else Doc.empty
      val defaultDoc = default.map(d => Docs.`=` <+> d.toDoc).getOrElse(Doc.empty)
      bind.toDoc <> varargDoc <> Docs.`:` <+> ty.toDoc <> defaultDoc
    }

    def cpy(
        bind: LocalVC[Term] = bind,
        ty: Term = ty,
        default: Option[Term] = default,
        vararg: Boolean = vararg,
        meta: OptionTermMeta = meta
    ): ThisTree =
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
        effects: EffectsMT[Term],
        meta: OptionTermMeta
    ): ThisTree
  }

  trait FunctionTypeC[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: FunctionTypeC[Term]

    def telescope: Vector[TelescopeTermC[Term]]

    def resultTy: Term

    def effects: EffectsMT[Term]

    def cons: FunctionTypeF[Term, ThisTree]

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
        effects: EffectsMT[Term] = effects,
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

    override def toDoc(using options: PrettierOptions): Doc = group(
      key.toDoc <+> Doc.text("=") <+> value.toDoc
    )

    def cpy(key: Term = key, value: Term = value, meta: OptionTermMeta = meta): ThisTree =
      cons.newObjectClauseValueTerm(key, value, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(clauses.map(_.toDoc))

    def cpy(clauses: Vector[ObjectClauseValueTermC[Term]] = clauses, meta: OptionTermMeta = meta): ThisTree =
      cons.newObjectTerm(clauses, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist("Object" </> Docs.`{`, Docs.`}`, ",")(
        fieldTypes.map(_.toDoc)
      )

    def cpy(
        fieldTypes: Vector[ObjectClauseValueTermC[Term]] = fieldTypes,
        exactFields: Boolean = exactFields,
        meta: OptionTermMeta = meta
    ): ThisTree =
      cons.newObjectType(fieldTypes, exactFields, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(fieldTypes = fieldTypes.map(g))
    )
  }

  @FunctionalInterface
  trait ListFF[Term <: TermT[Term], ThisTree <: ListFC[Term]] {
    def newListF(meta: OptionTermMeta): ThisTree
  }
  trait ListFC[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: ListFC[Term]

    def cons: ListFF[Term, ThisTree]
    override def toDoc(using options: PrettierOptions): Doc = "List"
    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newListF(meta)  
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }

  trait BuiltinT[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: BuiltinT[Term]

  }

  trait ConstructedT[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: ConstructedT[Term]
  }

  @FunctionalInterface
  trait ListTypeF[Term <: TermT[Term], ThisTree <: ListTypeC[Term]] {
    def newListType(ty: Term, meta: OptionTermMeta): ThisTree
  }

  trait ListTypeC[Term <: TermT[Term]] extends ConstructedT[Term] {
    override type ThisTree <: ListTypeC[Term]

    def ty: Term

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("List") <> Docs.`(` <> ty <> Docs.`)`
    def cons: ListTypeF[Term, ThisTree]

    def cpy(ty: Term = ty, meta: OptionTermMeta = meta): ThisTree = cons.newListType(ty, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(ty = f(ty)))
  }

  @FunctionalInterface
  trait UnionF[Term <: TermT[Term], ThisTree <: UnionC[Term]] {
    def newUnion(xs: NonEmptyVector[Term], meta: OptionTermMeta): ThisTree
  }

  trait UnionC[Term <: TermT[Term]] extends ConstructedT[Term] {
    override type ThisTree <: UnionC[Term]

    def xs: NonEmptyVector[Term]

    def cons: UnionF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist(Docs.`(`, Docs.`)`, "|")(xs)

    def cpy(xs: NonEmptyVector[Term] = xs, meta: OptionTermMeta = meta): ThisTree =
      cons.newUnion(xs, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(xs = xs.map(f))
    )
  }

  @FunctionalInterface
  trait IntersectionF[Term <: TermT[Term], ThisTree <: IntersectionC[Term]] {
    def newIntersection(xs: NonEmptyVector[Term], meta: OptionTermMeta): ThisTree
  }
  trait IntersectionC[Term <: TermT[Term]] extends ConstructedT[Term] {
    override type ThisTree <: IntersectionC[Term]

    def xs: NonEmptyVector[Term]

    def cons: IntersectionF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist(Docs.`(`, Docs.`)`, "&")(xs)

    def cpy(xs: NonEmptyVector[Term] = xs, meta: OptionTermMeta = meta): ThisTree = cons.newIntersection(xs, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(xs = xs.map(f)))
  }

  /** Effect needs to have reasonable equals and hashcode for simple comparison, whereas they are not requirements for other Terms
    */
  trait EffectT[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: EffectT[Term]
  }

  implicit inline def convert000[Term <: TermT[Term], LocalV <: LocalVC[Term]](x:Map[LocalVC[Term],Term]):Map[LocalV,Term] = x.asInstanceOf[Map[LocalV,Term]]

  @FunctionalInterface
  trait EffectsF[Term <: TermT[Term], ThisTree <: EffectsC[Term]] {
    def newEffects(effects: Map[LocalVC[Term], Term], meta: OptionTermMeta): ThisTree
  }

  trait EffectsC[Term <: TermT[Term]] extends EffectT[Term] {
    override type ThisTree <: EffectsC[Term]

    def effects: Map[LocalVC[Term], Term]

    def cons: EffectsF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.wrapperlist(Docs.`{`, Docs.`}`, ",")(effects.map { case (k, v) =>
        k.toDoc <+> Docs.`:` <+> v.toDoc
      })

    def isEmpty: Boolean = effects.isEmpty

    def nonEmpty: Boolean = effects.nonEmpty

    override def collectMeta: Vector[MetaTermC[Term]] =
      effects.flatMap((a, b) => a.collectMeta ++ b.collectMeta).toVector

    override def replaceMeta(f: MetaTermC[Term] => Term): ThisTree = cpy(effects = effects.map { case (a, b) =>
      (a.replaceMeta(f).asInstanceOf[LocalVC[Term]], b.replaceMeta(f))
    })
    def cpy(effects: Map[LocalVC[Term], Term] = effects, meta: OptionTermMeta = meta): ThisTree = cons.newEffects(effects, meta)

    override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(cpy(effects = effects.map { case (k, v) => (k, f(v)) }))
  }

  implicit inline def convertF[Term <: TermT[Term], MetaTerm <: MetaTermC[Term]](x: MetaTerm): MetaTermC[Term] = x.asInstanceOf[MetaTermC[Term]]

  implicit inline def convertF2[Term <: TermT[Term], MetaTerm <: MetaTermC[Term]](x: (MetaTerm=>Term)): (MetaTermC[Term]=>Term) = x.asInstanceOf[(MetaTermC[Term]=>Term)  ]
  @FunctionalInterface
  trait ExceptionEffectF[Term <: TermT[Term], ThisTree <: ExceptionEffectC[Term]] {
    def newExceptionEffect(meta: OptionTermMeta): ThisTree
  }

  // may raise an exception
  trait ExceptionEffectC[Term <: TermT[Term]] extends EffectT[Term] {
    override type ThisTree <: ExceptionEffectC[Term]

    def cons: ExceptionEffectF[Term, ThisTree]
    val name = "Exception"

    override def toDoc(using options: PrettierOptions): Doc = Doc.text(name)

    def cpy(meta: OptionTermMeta = meta): ThisTree = cons.newExceptionEffect(meta)
    override def descent(f: Term => Term, g: TreeMap[Term]): Term = this
  }
  // todo: may not terminate

  // todo:  whatever IO: console, file, network, ...

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

    override def toDoc(using options: PrettierOptions): Doc = {
      Doc.text("let ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
    }

    def cpy(localv: LocalVC[Term] = localv, value: Term = value, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
      cons.newLetStmt(localv, value, ty, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc = {
      Doc.text("def ") <> localv.toDoc <> Doc.text(": ") <> ty.toDoc <> Doc.text(" = ") <> value.toDoc
    }

    def cpy(localv: LocalVC[Term] = localv, value: Term = value, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
      cons.newDefStmt(localv, value, ty, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc = expr.toDoc

    def cpy(expr: Term = expr, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
      cons.newExprStmt(expr, ty, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("return") <+> value.toDoc

    def cpy(value: Term = value, meta: OptionTermMeta = meta): ThisTree =
      cons.newNonlocalOrLocalReturn(value, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(value = f(value))
    )
  }

  trait TupleTypeC[Term <: TermT[Term]] extends TypeTermT[Term] {
    override type ThisTree <: TupleTypeC[Term]

    def types: Vector[Term]

    override def toDoc(using options: PrettierOptions): Doc = {
      Doc.wrapperlist("Tuple" <> Docs.`[`, Docs.`]`, ",")(types)
    }

    def cpy(types: Vector[Term] = types, meta: OptionTermMeta = meta): ThisTree =
      cons.apply(types, meta)

    def cons: TupleTypeF[Term, ThisTree]

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc = {
      Doc.wrapperlist(Docs.`(`, Docs.`)`, ",")(values)
    }

    def cpy(values: Vector[Term] = values, meta: OptionTermMeta = meta): ThisTree =
      cons.apply(values, meta)

    def cons: TupleTermF[Term, ThisTree]

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc = {
      Doc.wrapperlist(Docs.`{`, Docs.`}`, ";")(
        (statements.map(_.toDoc) :+ result.toDoc)
      )
    }

    def cpy(statements: Vector[StmtTermT[Term]] = statements, result: Term = result, meta: OptionTermMeta = meta): ThisTree =
      cons.newBlockTerm(statements, result, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(
        statements = statements.map(g),
        result = f(result)
      )
    )
  }

  @FunctionalInterface
  trait AnnotationF[Term <: TermT[Term], ThisTree <: AnnotationC[Term]] {
    def newAnnotation(term: Term, ty: Option[Term], effects: Option[EffectsMT[Term]], meta: OptionTermMeta): ThisTree
  }

  trait AnnotationC[Term <: TermT[Term]] extends UnevalT[Term] {
    override type ThisTree <: AnnotationC[Term]

    def term: Term

    def ty: Option[Term]

    def effects: Option[EffectsMT[Term]]

    def cons: AnnotationF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc = {
      val tyDoc = ty.map(Docs.`:` <+> _.toDoc).getOrElse(Doc.empty)
      val effectsDoc = effects.map(Docs.`/` <+> _.toDoc).getOrElse(Doc.empty)
      term.toDoc <> tyDoc <> effectsDoc
    }

    def cpy(term: Term = term, ty: Option[Term] = ty, effects: Option[EffectsMT[Term]] = effects, meta: OptionTermMeta = meta): ThisTree =
      cons.newAnnotation(term, ty, effects, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(
        term = f(term),
        ty = ty.map(f),
        effects = effects.map(x => g(x).asInstanceOf[EffectsMT[Term]])
      )
    )
  }

  trait FieldTermC[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: FieldTermC[Term]

    def name: Name

    def ty: Term

    def cons: FieldTermF[Term, ThisTree]

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text(name) <> Doc.text(": ") <> ty.toDoc

    def cpy(name: Name = name, ty: Term = ty, meta: OptionTermMeta = meta): ThisTree =
      cons.apply(name, ty, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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


    def uniqId: UniqidOf[TypeDefinitionT[Term]]

    override def switchUniqId(r: UReplacer): TypeDefinitionT[Term]

    override type ThisTree <: TypeDefinitionT[Term]
  }

  implicit inline def uniqOb[Term <: TermT[Term], ThisTree <: ObjectStmtTermC[Term]](x:  UniqidOf[ObjectStmtTermC[Term]]): UniqidOf[ThisTree] = x.asInstanceOf[UniqidOf[ThisTree]]

  @FunctionalInterface
  trait ObjectStmtTermF[Term <: TermT[Term], ThisTree <: ObjectStmtTermC[Term]] {
    def newObjectStmt(name: Name, uniqId: UniqidOf[ObjectStmtTermC[Term]], extendsClause: Option[Term], body: Option[BlockTermC[Term]], meta: OptionTermMeta): ThisTree
  }

  trait ObjectStmtTermC[Term <: TermT[Term]] extends TypeDefinitionT[Term] {
    override type ThisTree <: ObjectStmtTermC[Term]

    def name: Name

    def uniqId: UniqidOf[ObjectStmtTermC[Term]]

    def extendsClause: Option[Term]

    def body: Option[BlockTermC[Term]]
    override def switchUniqId(r: UReplacer): ThisTree = cpy(uniqId = r(uniqId))

    override def toDoc(using options: PrettierOptions): Doc = {
      val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
      val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
      Doc.text("object") <+> Doc.text(name) <+> extendsDoc <+> bodyDoc
    }

    def cons: ObjectStmtTermF[Term, ThisTree]

    def cpy(name: Name = name, uniqId: UniqidOf[ObjectStmtTermC[Term]] = uniqId, extendsClause: Option[Term] = extendsClause, body: Option[BlockTermC[Term]] = body, meta: OptionTermMeta = meta): ThisTree =
      cons.newObjectStmt(name, uniqId, extendsClause, body, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(extendsClause = extendsClause.map(g), body = body.map(g))
    )
  }

  implicit inline def uniqInterface[Term <: TermT[Term], ThisTree <: InterfaceStmtTermC[Term]](x:  UniqidOf[InterfaceStmtTermC[Term]]): UniqidOf[ThisTree] = x.asInstanceOf[UniqidOf[ThisTree]]

  @FunctionalInterface
  trait InterfaceStmtTermF[Term <: TermT[Term], ThisTree <: InterfaceStmtTermC[Term]] {
    def newInterfaceStmt(name: Name, uniqId: UniqidOf[InterfaceStmtTermC[Term]], extendsClause: Option[Term], body: Option[BlockTermC[Term]], meta: OptionTermMeta): ThisTree
  }

  trait InterfaceStmtTermC[Term <: TermT[Term]] extends TypeDefinitionT[Term] with StmtTermT[Term] {
    override type ThisTree <: InterfaceStmtTermC[Term]

    def name: Name
    def uniqId: UniqidOf[InterfaceStmtTermC[Term]]
    def extendsClause: Option[Term]
    def body: Option[BlockTermC[Term]]



    override def toDoc(using options: PrettierOptions): Doc = {
      val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
      val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
      group(
        Doc.text("interface ") <> Doc.text(name.toString) <> extendsDoc <> bodyDoc
      )
    }

    def cons: InterfaceStmtTermF[Term, ThisTree]

    def cpy(name: Name = name, uniqId: UniqidOf[InterfaceStmtTermC[Term]] = uniqId, extendsClause: Option[Term] = extendsClause, body: Option[BlockTermC[Term]] = body, meta: OptionTermMeta = meta): ThisTree =
      cons.newInterfaceStmt(name, uniqId, extendsClause, body, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(extendsClause = extendsClause.map(g), body = body.map(g))
    )
  }

  implicit inline def uniqTrait[Term <: TermT[Term], ThisTree <: TraitStmtTermC[Term]](x:  UniqidOf[TraitStmtTermC[Term]]): UniqidOf[ThisTree] = x.asInstanceOf[UniqidOf[ThisTree]]

  @FunctionalInterface
  trait TraitStmtTermF[Term <: TermT[Term], ThisTree <: TraitStmtTermC[Term]] {
    def newTraitStmt(name: Name, uniqId: UniqidOf[TraitStmtTermC[Term]], extendsClause: Option[Term], body: Option[BlockTermC[Term]], meta: OptionTermMeta): ThisTree
  }

  trait TraitStmtTermC[Term <: TermT[Term]] extends TypeDefinitionT[Term] with StmtTermT[Term] {
    override type ThisTree <: TraitStmtTermC[Term]


    def name: Name
    def uniqId: UniqidOf[TraitStmtTermC[Term]]
    def extendsClause: Option[Term]
    def body: Option[BlockTermC[Term]]

    override def switchUniqId(r: UReplacer): ThisTree = cpy(uniqId = r(uniqId))


    override def toDoc(using options: PrettierOptions): Doc = {
      val extendsDoc = extendsClause.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
      val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
      group(
        Doc.text("trait ") <> Doc.text(name) <> extendsDoc <> bodyDoc
      )
    }

    def cons: TraitStmtTermF[Term, ThisTree]
    def cpy(name: Name = name, uniqId: UniqidOf[TraitStmtTermC[Term]] = uniqId, extendsClause: Option[Term] = extendsClause, body: Option[BlockTermC[Term]] = body, meta: OptionTermMeta = meta): ThisTree =
      cons.newTraitStmt(name, uniqId, extendsClause, body, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(extendsClause = extendsClause.map(g), body = body.map(g))
    )
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

    override def switchUniqId(r: UReplacer): ThisTree = cpy(uniqId = r(uniqId))

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

    override def toDoc(using options: PrettierOptions): Doc =
      group("ObjectCall" <+> objectRef.toDoc)

    def cpy(objectRef: Term = objectRef, meta: OptionTermMeta = meta): ThisTree =
      cons.newObjectCallTerm(objectRef, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
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

    override def toDoc(using options: PrettierOptions): Doc =
      Doc.text("ObjectType(") <> objectDef.name.toDoc <> Doc.text(")")

    def cpy(objectDef: ObjectStmtTerm = objectDef, meta: OptionTermMeta = meta): ThisTree =
      cons.newObjectTypeTerm(objectDef, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(objectDef = g(objectDef))
    )
  }

  @FunctionalInterface
  trait TelescopeTermF[Term <: TermT[Term], ThisTree <: TelescopeTermC[Term]] {
    def newTelescope(
        args: Vector[ArgTermC[Term]],
        implicitly: Boolean,
        meta: OptionTermMeta
    ): ThisTree
  }

  @FunctionalInterface
  trait RecordConstructorCallTermF[Term <: TermT[Term], ThisTree <: RecordConstructorCallTermC[Term]] {
    def newRecordConstructorCallTerm(recordName: Name, args: Vector[Term], meta: OptionTermMeta): ThisTree
  }

  trait RecordConstructorCallTermC[Term <: TermT[Term]] extends WHNFT[Term] {
    override type ThisTree <: RecordConstructorCallTermC[Term]

    def recordName: Name

    def args: Vector[Term]

    def cons: RecordConstructorCallTermF[Term, ThisTree]
    

    override def toDoc(using options: PrettierOptions): Doc = {
      val argsDoc = Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,`)(args.map(_.toDoc))
      Doc.text(recordName) <> argsDoc
    }

    def cpy(recordName: Name = recordName, args: Vector[Term] = args, meta: OptionTermMeta = meta): ThisTree =
      cons.newRecordConstructorCallTerm(recordName, args, meta)

     override def descent(f: Term => Term, g: TreeMap[Term]): Term = thisOr(
      cpy(args = args.map(f))
    )
  }
}
