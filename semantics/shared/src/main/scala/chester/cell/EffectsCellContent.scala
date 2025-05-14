package chester.cell

import chester.syntax.core.*
import chester.utils.cell.*
import chester.utils.elab.Cell

type CellEffects = Cell[Effects, Effects, EffectsCellContent]
trait EffectsCellContent extends CellContentRW[Effects] {}

case class DynamicEffectsCellContent(effects: Map[LocalV, Term] = Map.empty)
    extends BaseMapCell[LocalV, Term]
    with EffectsCellContent
    with UnstableCellContent[Effects, Effects]
    with NoFill[Effects, Effects] {
  override def add(key: LocalV, value: Term): DynamicEffectsCellContent = {
    require(!effects.contains(key))
    copy(effects = effects.updated(key, value))
  }

  override def readUnstable: Option[Effects] = Some(Effects(effects, None))
}

case class FixedEffectsCellContent(effects: Effects) extends EffectsCellContent with NoFill[Effects, Effects] {
  override def readStable: Option[Effects] = Some(effects)
}
