package chester.error

trait SpanOptional extends Any {
  def span0: Option[Span]
}

trait SpanOptional1 extends Any with SpanOptional {
  def span: Option[Span]
  override def span0: Option[Span] = span
}
