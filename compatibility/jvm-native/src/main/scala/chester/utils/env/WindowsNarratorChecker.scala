package chester.utils.env

// scalafix:off
import scala.sys.process.*
import scala.util.Try
import com.eed3si9n.ifdef.*

object WindowsNarratorChecker {
  @ifndef("scalaNativeForTermux")
  def apply(): Boolean = {
    val processList = Try("tasklist".!!).getOrElse("")

    processList.linesIterator.exists(_.startsWith("Narrator.exe"))
  }
  @ifdef("scalaNativeForTermux")
  def apply(): Boolean = false
}
