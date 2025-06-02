package chester.error

trait SpanRequired extends Any with SpanOptional0 {
  def span: Span
  override def span0: Option[Span] = Some(span)

}
