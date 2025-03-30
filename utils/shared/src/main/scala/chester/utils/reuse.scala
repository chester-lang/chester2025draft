package chester.utils

inline def reuse[T](inline origin: T, inline newOne: T): T =
  if (origin == newOne) origin
  else newOne
