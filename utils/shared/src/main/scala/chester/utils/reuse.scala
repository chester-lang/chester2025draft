package chester.utils

inline def reuse[A, B <: A](inline origin: A, inline newOne: B): B =
  if (origin == newOne) origin.asInstanceOf[B]
  else newOne
