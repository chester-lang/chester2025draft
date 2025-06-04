package chester.doc.consts

import chester.doc.*
import chester.utils.doc.*

object Colors {
  def REPLPrompt: Style = Foreground.LightBlue ++ Styling.BoldOn
}

class ColorProfile {
  def literalColor(using DocConf): Style =
    if (LightMode.get) Foreground.Red else Foreground.LightRed

  def typeColor(using DocConf): Style =
    if (LightMode.get) Foreground.Blue else Foreground.LightBlue
}

case object ColorProfile extends DocConfKey[ColorProfile] {

  val default = new ColorProfile

  def literalColor(using DocConf): Style = get.literalColor

  def typeColor(using DocConf): Style = get.typeColor

}
