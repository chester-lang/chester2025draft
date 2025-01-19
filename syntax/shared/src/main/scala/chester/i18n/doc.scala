package chester.i18n

import chester.utils.doc.{PrettierOptions, PrettierOptionsKey}

case object LanguageKey extends PrettierOptionsKey[Language] {
  val default: Language = Language.from("en_NZ")
}

implicit def languageInPretty(using PrettierOptions): Language =
  LanguageKey.get
