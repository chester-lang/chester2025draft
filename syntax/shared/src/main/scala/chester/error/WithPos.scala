package chester.error

trait WithPos {
  def sourcePos: Option[SourcePos]
}
