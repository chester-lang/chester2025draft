package chester.utils.propagator

trait ProvideImpl extends ProvideCellId {
  def stateAbilityImpl[Ability]: StateAbility[Ability]
}
