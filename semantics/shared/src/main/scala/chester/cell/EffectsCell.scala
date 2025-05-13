package chester.cell

import chester.syntax.core.*
import chester.utils.cell.*

trait EffectsCell extends CellRW[Effects] {
}


case class DynamicEffectsCell(effects: Map[LocalV, Term] = Map.empty)
  extends BaseMapCell[LocalV, Term]
    with EffectsCell
    with UnstableCell[Effects, Effects]
    with NoFill[Effects, Effects] {
  override def add(key: LocalV, value: Term): DynamicEffectsCell = {
    require(!effects.contains(key))
    copy(effects = effects.updated(key, value))
  }

  override def readUnstable: Option[Effects] = Some(Effects(effects, None))
}

case class FixedEffectsCell(effects: Effects) extends EffectsCell with NoFill[Effects, Effects] {
  override def readStable: Option[Effects] = Some(effects)
}
