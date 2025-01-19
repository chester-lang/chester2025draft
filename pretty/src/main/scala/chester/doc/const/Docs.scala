package chester.doc.const

import chester.utils.doc.{Doc, PrettierOptions}

object Docs {
  def `{`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("begin ") else Doc.text("{")
  def `}`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("end ") else Doc.text("}")
  def `[`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("begin list ") else Doc.text("[")
  def `]`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("end list ") else Doc.text("]")
  def `(`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("beginTuple ") else Doc.text("(")
  def `)`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("endTuple ") else Doc.text(")")
  def `->`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("to ") else Doc.text("->")
  def `:`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("is ") else Doc.text(":")
  def `...`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("ellipsis ") else Doc.text("...")
  def `=`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("equals ") else Doc.text("=")
  def `,`(using  PrettierOptions): Doc = Doc.text(",")
  def `/`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("slash ") else Doc.text("/")
  def `=>`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("returns ") else Doc.text("=>")
  def `.`(using  PrettierOptions): Doc = Doc.text(".")
  def `;`(using  PrettierOptions): Doc = Doc.text(";")
  def `<:`(using  PrettierOptions): Doc =
    if (ReplaceBracketsWithWord.get) Doc.text("isSubtypeOf ") else Doc.text("<:")
  val `with`: Doc = Doc.text("with")
}
