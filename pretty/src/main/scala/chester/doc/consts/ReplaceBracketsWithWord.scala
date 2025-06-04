package chester.doc.consts

import chester.utils.doc.DocConfKey

// will work better with Windows Narrator on Windows Terminal
case object ReplaceBracketsWithWord extends DocConfKey[Boolean] {
  val default: Boolean = false

}
