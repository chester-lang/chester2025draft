package chester.i18n

import chester.utils.doc.{DocConf, DocConfKey}

import scala.language.implicitConversions

case object LanguageKey extends DocConfKey[Language] {
  val default: Language = Language.from("en_NZ")
}

implicit def languageInPretty(using DocConf): Language =
  LanguageKey.get
