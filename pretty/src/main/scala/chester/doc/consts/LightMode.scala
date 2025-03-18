package chester.doc.consts

import chester.utils.doc.PrettierOptionsKey

case object LightMode extends PrettierOptionsKey[Boolean] {
  val default: Boolean = false
}
