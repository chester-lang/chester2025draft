package chester.error

trait WithPos extends Any {
  def sourcePos: Option[Span]
}
