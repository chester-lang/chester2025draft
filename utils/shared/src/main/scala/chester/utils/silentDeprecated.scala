package chester.utils

import scala.annotation.meta.{getter, setter}

/** something depreacted but I don't want to see warning messages */
@getter @setter
class silentDeprecated(message: String = "", since: String = "") extends scala.annotation.ConstantAnnotation
