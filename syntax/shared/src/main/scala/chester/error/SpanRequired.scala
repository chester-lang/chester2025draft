package chester.error

trait SpanRequired extends Any with SpanOptional {
  def span: Span
  override def span0: Option[Span] = Some(span)

}
