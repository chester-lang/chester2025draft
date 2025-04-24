package chester.utils.doc
import scala.language.implicitConversions

trait PrettierOptionsKey[T] {
  def default: T

  def get(using options: PrettierOptions): T = options.get(this)
}

case class PrettierOptions(x: Map[PrettierOptionsKey[?], Any]) extends AnyVal {
  private inline def options: Map[PrettierOptionsKey[?], Any] = x
  def get[T](key: PrettierOptionsKey[T]): T = {
    val default: T = key.default
    getOrElse__[T](key, default)
  }

  private def getOption[T](key: PrettierOptionsKey[T]): Option[T] =
    options.get(key).map(_.asInstanceOf[T])

  private def getOrElse__[T](key: PrettierOptionsKey[T], default: T): T =
    getOption(key).getOrElse(default)

  def updated[T](key: PrettierOptionsKey[T], value: T): PrettierOptions =
    PrettierOptions(options.updated(key, value))
}

case class PrettierKeyValue[T](key: PrettierOptionsKey[T], value: T)

implicit def tuple2PrettyKeyValue[T](
    tuple: (PrettierOptionsKey[T], T)
): PrettierKeyValue[T] = PrettierKeyValue(tuple._1, tuple._2)
implicit def prettyKeyValue2Tuple[T](
    kv: PrettierKeyValue[T]
): (PrettierOptionsKey[T], T) = (kv.key, kv.value)

object PrettierOptions {
  val Default: PrettierOptions = PrettierOptions(Map.empty)

  def apply(options: PrettierKeyValue[?]*): PrettierOptions = PrettierOptions(
    options.map(prettyKeyValue2Tuple).toMap
  )
}
