package chester.syntax.core
import chester.doc.consts.Docs
import chester.uniqid.*
import chester.utils.doc.*
import upickle.default.*

case class Judge(wellTyped: Term, ty: Term, effects: Effects = Effects.Empty) extends ContainsUniqid with ToDoc derives ReadWriter {
  def substitute(from: TermWithUniqid, to: Term): Judge = Judge(
    wellTyped.substitute(from, to),
    ty.substitute(from, to),
    effects.substitute(from, to).asInstanceOf[Effects]
  )

  def collectMeta: Vector[MetaTerm[?]] =
    wellTyped.collectMeta ++ ty.collectMeta ++ effects.collectMeta
  def replaceMeta(f: MetaTerm[?] => Term): Judge =
    Judge(wellTyped.replaceMeta(f), ty.replaceMeta(f), effects.replaceMeta(f).asInstanceOf[Effects])

  override def collectU(collector: UCollector): Unit = {
    wellTyped.collectU(collector)
    ty.collectU(collector)
    effects.collectU(collector)
  }

  override def replaceU(reranger: UReplacer): Judge =
    copy(
      wellTyped.replaceU(reranger),
      ty.replaceU(reranger),
      effects.replaceU(reranger).asInstanceOf[Effects]
    )

  override def toDoc(using PrettierOptions): Doc =
    Docs.`(` <> wellTyped.toDoc <> Docs.`,` <> ty.toDoc <> Docs.`,` <> effects.toDoc <> Docs.`)`
}
