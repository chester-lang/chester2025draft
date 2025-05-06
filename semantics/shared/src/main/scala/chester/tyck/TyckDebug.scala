package chester.tyck

import chester.utils.Parameter

val TyckDebug = Parameter.withDefault(false)


private def debug(msg: => String): Unit = if (TyckDebug.get) println(s"[DEBUG] $msg")

