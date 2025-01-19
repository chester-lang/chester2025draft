package chester.doc.const

import chester.doc.*
import chester.utils.doc.*

object Colors {
  def REPLPrompt: Style = Foreground.LightBlue ++ Styling.BoldOn
}

class ColorProfile {
  def literalColor(using PrettierOptions): Style =
    if (LightMode.get) Foreground.Red else Foreground.LightRed

  def typeColor(using PrettierOptions): Style =
    if (LightMode.get) Foreground.Blue else Foreground.LightBlue
}

case object ColorProfile extends PrettierOptionsKey[ColorProfile] {

  val default = new ColorProfile

  def literalColor(using PrettierOptions): Style = get.literalColor

  def typeColor(using PrettierOptions): Style = get.typeColor

}
