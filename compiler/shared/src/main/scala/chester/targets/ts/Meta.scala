package chester.targets.ts

import chester.error.*

import upickle.default.*

case class Meta(span: Option[Span]) extends SpanOptional derives ReadWriter
