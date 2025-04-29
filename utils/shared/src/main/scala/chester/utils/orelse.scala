package chester.utils

extension [A, B](x: Either[A, B]) {
  inline def orelse(y: Option[B]): Either[A, B] = x match {
    case r @ Right(_) => r
    case l @ Left(_) =>
      y match {
        case Some(r) => Right(r)
        case None    => l
      }
  }
}
