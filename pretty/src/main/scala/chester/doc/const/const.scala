package chester.doc.const

import chester.doc.*
import chester.utils.doc.*

object Colors {
  def REPLPrompt: Style = Foreground.LightBlue ++ Styling.BoldOn
}

class ColorProfile {
  def literalColor(using options: PrettierOptions): Style =
    if (LightMode.get) Foreground.Red else Foreground.LightRed

  def typeColor(using options: PrettierOptions): Style =
    if (LightMode.get) Foreground.Blue else Foreground.LightBlue
}

case object ColorProfile extends PrettierOptionsKey[ColorProfile] {

  val default = new ColorProfile

  def literalColor(using options: PrettierOptions): Style = get.literalColor

  def typeColor(using options: PrettierOptions): Style = get.typeColor

}
