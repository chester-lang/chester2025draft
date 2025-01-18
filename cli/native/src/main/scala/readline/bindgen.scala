/*
tee readline.h << EOF
#include <stdio.h>

$(cat /usr/include/readline/readline.h)
$(cat /usr/include/readline/history.h)
EOF

LD_LIBRARY_PATH=/usr/lib/llvm-17/lib/ ~/Downloads/sn-bindgen-x86_64-pc-linux --package readline --header readline.h --scala > ~/Downloads/readline.scala

ALSO REPLACED macro with `macro`
 */
package readline

import _root_.scala.scalanative._
import _root_.scala.scalanative.unsafe._
import _root_.scala.scalanative.unsigned._

object predef:
  private[readline] trait CEnumU[T](using eq: T =:= UInt):
    given Tag[T] = Tag.UInt.asInstanceOf[Tag[T]]
    extension (inline t: T)
      inline def int: CInt = eq.apply(t).toInt
      inline def uint: CUnsignedInt = eq.apply(t)
      inline def value: CUnsignedInt = eq.apply(t)

object enumerations:
  import predef.*

  /** [bindgen] header: readline.h
    */
  opaque type undo_code = CUnsignedInt
  object undo_code extends CEnumU[undo_code]:
    given _tag: Tag[undo_code] = Tag.UInt
    inline def define(inline a: Long): undo_code = a.toUInt
    val UNDO_DELETE = define(0)
    val UNDO_INSERT = define(1)
    val UNDO_BEGIN = define(2)
    val UNDO_END = define(3)
    inline def getName(inline value: undo_code): Option[String] =
      inline value match
        case UNDO_DELETE => Some("UNDO_DELETE")
        case UNDO_INSERT => Some("UNDO_INSERT")
        case UNDO_BEGIN  => Some("UNDO_BEGIN")
        case UNDO_END    => Some("UNDO_END")
        case _           => _root_.scala.None
    extension (a: undo_code)
      inline def &(b: undo_code): undo_code = a & b
      inline def |(b: undo_code): undo_code = a | b
      inline def is(b: undo_code): Boolean = (a & b) == b

object aliases:
  import _root_.readline.structs.*
  type FILE = libc.stdio.FILE
  object FILE:
    val _tag: Tag[FILE] = summon[Tag[libc.stdio.FILE]]
    inline def apply(inline o: libc.stdio.FILE): FILE = o
    extension (v: FILE) inline def value: libc.stdio.FILE = v

  /** [bindgen] header: /usr/include/readline/keymaps.h
    */
  opaque type Keymap = Ptr[KEYMAP_ENTRY]
  object Keymap:
    given _tag: Tag[Keymap] = Tag.Ptr[KEYMAP_ENTRY](KEYMAP_ENTRY._tag)
    inline def apply(inline o: Ptr[KEYMAP_ENTRY]): Keymap = o
    extension (v: Keymap) inline def value: Ptr[KEYMAP_ENTRY] = v

  /** [bindgen] header: readline.h
    */
  opaque type histdata_t = Ptr[Byte]
  object histdata_t:
    given _tag: Tag[histdata_t] = Tag.Ptr(Tag.Byte)
    inline def apply(inline o: Ptr[Byte]): histdata_t = o
    extension (v: histdata_t) inline def value: Ptr[Byte] = v

  /** [bindgen] header: /usr/include/readline/rltypedefs.h
    */
  type rl_command_func_t = CFuncPtr2[CInt, CInt, CInt]
  object rl_command_func_t:
    given _tag: Tag[rl_command_func_t] =
      Tag.materializeCFuncPtr2[CInt, CInt, CInt]
    inline def apply(inline o: CFuncPtr2[CInt, CInt, CInt]): rl_command_func_t =
      o
    extension (v: rl_command_func_t) inline def value: CFuncPtr2[CInt, CInt, CInt] = v

  /** [bindgen] header: /usr/include/readline/rltypedefs.h
    */
  type rl_compentry_func_t = CFuncPtr2[CString, CInt, CString]
  object rl_compentry_func_t:
    given _tag: Tag[rl_compentry_func_t] =
      Tag.materializeCFuncPtr2[CString, CInt, CString]
    inline def apply(
        inline o: CFuncPtr2[CString, CInt, CString]
    ): rl_compentry_func_t = o
    extension (v: rl_compentry_func_t) inline def value: CFuncPtr2[CString, CInt, CString] = v

  /** [bindgen] header: /usr/include/readline/rltypedefs.h
    */
  type rl_compignore_func_t = CFuncPtr1[Ptr[CString], CInt]
  object rl_compignore_func_t:
    given _tag: Tag[rl_compignore_func_t] =
      Tag.materializeCFuncPtr1[Ptr[CString], CInt]
    inline def apply(
        inline o: CFuncPtr1[Ptr[CString], CInt]
    ): rl_compignore_func_t = o
    extension (v: rl_compignore_func_t) inline def value: CFuncPtr1[Ptr[CString], CInt] = v

  /** [bindgen] header: /usr/include/readline/rltypedefs.h
    */
  type rl_completion_func_t = CFuncPtr3[CString, CInt, CInt, Ptr[CString]]
  object rl_completion_func_t:
    given _tag: Tag[rl_completion_func_t] =
      Tag.materializeCFuncPtr3[CString, CInt, CInt, Ptr[CString]]
    inline def apply(
        inline o: CFuncPtr3[CString, CInt, CInt, Ptr[CString]]
    ): rl_completion_func_t = o
    extension (v: rl_completion_func_t) inline def value: CFuncPtr3[CString, CInt, CInt, Ptr[CString]] = v

  /** [bindgen] header: /usr/include/readline/rltypedefs.h
    */
  type rl_vcpfunc_t = CFuncPtr1[CString, Unit]
  object rl_vcpfunc_t:
    given _tag: Tag[rl_vcpfunc_t] = Tag.materializeCFuncPtr1[CString, Unit]
    inline def apply(inline o: CFuncPtr1[CString, Unit]): rl_vcpfunc_t = o
    extension (v: rl_vcpfunc_t) inline def value: CFuncPtr1[CString, Unit] = v

  type size_t = libc.stddef.size_t
  object size_t:
    val _tag: Tag[size_t] = summon[Tag[libc.stddef.size_t]]
    inline def apply(inline o: libc.stddef.size_t): size_t = o
    extension (v: size_t) inline def value: libc.stddef.size_t = v

  type time_t = posix.sys.types.time_t
  object time_t:
    val _tag: Tag[time_t] = summon[Tag[posix.sys.types.time_t]]
    inline def apply(inline o: posix.sys.types.time_t): time_t = o
    extension (v: time_t) inline def value: posix.sys.types.time_t = v

object structs:
  import _root_.readline.aliases.*
  import _root_.readline.enumerations.*

  /** [bindgen] header: readline.h
    */
  opaque type FUNMAP = CStruct2[CString, Ptr[rl_command_func_t]]
  object FUNMAP:
    given _tag: Tag[FUNMAP] =
      Tag.materializeCStruct2Tag[CString, Ptr[rl_command_func_t]]
    def apply()(using Zone): Ptr[FUNMAP] =
      scala.scalanative.unsafe.alloc[FUNMAP](1)
    def apply(name: CString, function: Ptr[rl_command_func_t])(using
        Zone
    ): Ptr[FUNMAP] =
      val ____ptr = apply()
      (!____ptr).name = name
      (!____ptr).function = function
      ____ptr
    extension (struct: FUNMAP)
      def name: CString = struct._1
      def name_=(value: CString): Unit = !struct.at1 = value
      def function: Ptr[rl_command_func_t] = struct._2
      def function_=(value: Ptr[rl_command_func_t]): Unit = !struct.at2 = value

  /** [bindgen] header: readline.h
    */
  opaque type HISTORY_STATE =
    CStruct5[Ptr[Ptr[HIST_ENTRY]], CInt, CInt, CInt, CInt]
  object HISTORY_STATE:
    given _tag: Tag[HISTORY_STATE] =
      Tag.materializeCStruct5Tag[Ptr[Ptr[HIST_ENTRY]], CInt, CInt, CInt, CInt]
    def apply()(using Zone): Ptr[HISTORY_STATE] =
      scala.scalanative.unsafe.alloc[HISTORY_STATE](1)
    def apply(
        entries: Ptr[Ptr[HIST_ENTRY]],
        offset: CInt,
        length: CInt,
        size: CInt,
        flags: CInt
    )(using Zone): Ptr[HISTORY_STATE] =
      val ____ptr = apply()
      (!____ptr).entries = entries
      (!____ptr).offset = offset
      (!____ptr).length = length
      (!____ptr).size = size
      (!____ptr).flags = flags
      ____ptr
    extension (struct: HISTORY_STATE)
      def entries: Ptr[Ptr[HIST_ENTRY]] = struct._1
      def entries_=(value: Ptr[Ptr[HIST_ENTRY]]): Unit = !struct.at1 = value
      def offset: CInt = struct._2
      def offset_=(value: CInt): Unit = !struct.at2 = value
      def length: CInt = struct._3
      def length_=(value: CInt): Unit = !struct.at3 = value
      def size: CInt = struct._4
      def size_=(value: CInt): Unit = !struct.at4 = value
      def flags: CInt = struct._5
      def flags_=(value: CInt): Unit = !struct.at5 = value

  /** [bindgen] header: readline.h
    */
  opaque type HIST_ENTRY = CStruct3[CString, CString, histdata_t]
  object HIST_ENTRY:
    given _tag: Tag[HIST_ENTRY] =
      Tag.materializeCStruct3Tag[CString, CString, histdata_t]
    def apply()(using Zone): Ptr[HIST_ENTRY] =
      scala.scalanative.unsafe.alloc[HIST_ENTRY](1)
    def apply(line: CString, timestamp: CString, data: histdata_t)(using
        Zone
    ): Ptr[HIST_ENTRY] =
      val ____ptr = apply()
      (!____ptr).line = line
      (!____ptr).timestamp = timestamp
      (!____ptr).data = data
      ____ptr
    extension (struct: HIST_ENTRY)
      def line: CString = struct._1
      def line_=(value: CString): Unit = !struct.at1 = value
      def timestamp: CString = struct._2
      def timestamp_=(value: CString): Unit = !struct.at2 = value
      def data: histdata_t = struct._3
      def data_=(value: histdata_t): Unit = !struct.at3 = value

  /** [bindgen] header: /usr/include/readline/keymaps.h
    */
  opaque type KEYMAP_ENTRY = CStruct2[CChar, Ptr[rl_command_func_t]]
  object KEYMAP_ENTRY:
    given _tag: Tag[KEYMAP_ENTRY] =
      Tag.materializeCStruct2Tag[CChar, Ptr[rl_command_func_t]]
    def apply()(using Zone): Ptr[KEYMAP_ENTRY] =
      scala.scalanative.unsafe.alloc[KEYMAP_ENTRY](1)
    def apply(`type`: CChar, function: Ptr[rl_command_func_t])(using
        Zone
    ): Ptr[KEYMAP_ENTRY] =
      val ____ptr = apply()
      (!____ptr).`type` = `type`
      (!____ptr).function = function
      ____ptr
    extension (struct: KEYMAP_ENTRY)
      def `type`: CChar = struct._1
      def type_=(value: CChar): Unit = !struct.at1 = value
      def function: Ptr[rl_command_func_t] = struct._2
      def function_=(value: Ptr[rl_command_func_t]): Unit = !struct.at2 = value

  /** [bindgen] header: readline.h
    */
  opaque type UNDO_LIST = CStruct5[Ptr[Byte], CInt, CInt, CString, undo_code]
  object UNDO_LIST:
    given _tag: Tag[UNDO_LIST] =
      Tag.materializeCStruct5Tag[Ptr[Byte], CInt, CInt, CString, undo_code]
    def apply()(using Zone): Ptr[UNDO_LIST] =
      scala.scalanative.unsafe.alloc[UNDO_LIST](1)
    def apply(
        next: Ptr[undo_list],
        start: CInt,
        end: CInt,
        text: CString,
        what: undo_code
    )(using Zone): Ptr[UNDO_LIST] =
      val ____ptr = apply()
      (!____ptr).next = next
      (!____ptr).start = start
      (!____ptr).end = end
      (!____ptr).text = text
      (!____ptr).what = what
      ____ptr
    extension (struct: UNDO_LIST)
      def next: Ptr[undo_list] = struct._1.asInstanceOf[Ptr[undo_list]]
      def next_=(value: Ptr[undo_list]): Unit = !struct.at1 = value.asInstanceOf[Ptr[Byte]]
      def start: CInt = struct._2
      def start_=(value: CInt): Unit = !struct.at2 = value
      def end: CInt = struct._3
      def end_=(value: CInt): Unit = !struct.at3 = value
      def text: CString = struct._4
      def text_=(value: CString): Unit = !struct.at4 = value
      def what: undo_code = struct._5
      def what_=(value: undo_code): Unit = !struct.at5 = value

  /** [bindgen] header: readline.h
    */
  opaque type _funmap = CStruct2[CString, Ptr[rl_command_func_t]]
  object _funmap:
    given _tag: Tag[_funmap] =
      Tag.materializeCStruct2Tag[CString, Ptr[rl_command_func_t]]
    def apply()(using Zone): Ptr[_funmap] =
      scala.scalanative.unsafe.alloc[_funmap](1)
    def apply(name: CString, function: Ptr[rl_command_func_t])(using
        Zone
    ): Ptr[_funmap] =
      val ____ptr = apply()
      (!____ptr).name = name
      (!____ptr).function = function
      ____ptr
    extension (struct: _funmap)
      def name: CString = struct._1
      def name_=(value: CString): Unit = !struct.at1 = value
      def function: Ptr[rl_command_func_t] = struct._2
      def function_=(value: Ptr[rl_command_func_t]): Unit = !struct.at2 = value

  /** [bindgen] header: readline.h
    */
  opaque type _hist_entry = CStruct3[CString, CString, histdata_t]
  object _hist_entry:
    given _tag: Tag[_hist_entry] =
      Tag.materializeCStruct3Tag[CString, CString, histdata_t]
    def apply()(using Zone): Ptr[_hist_entry] =
      scala.scalanative.unsafe.alloc[_hist_entry](1)
    def apply(line: CString, timestamp: CString, data: histdata_t)(using
        Zone
    ): Ptr[_hist_entry] =
      val ____ptr = apply()
      (!____ptr).line = line
      (!____ptr).timestamp = timestamp
      (!____ptr).data = data
      ____ptr
    extension (struct: _hist_entry)
      def line: CString = struct._1
      def line_=(value: CString): Unit = !struct.at1 = value
      def timestamp: CString = struct._2
      def timestamp_=(value: CString): Unit = !struct.at2 = value
      def data: histdata_t = struct._3
      def data_=(value: histdata_t): Unit = !struct.at3 = value

  /** [bindgen] header: readline.h
    */
  opaque type _hist_state =
    CStruct5[Ptr[Ptr[HIST_ENTRY]], CInt, CInt, CInt, CInt]
  object _hist_state:
    given _tag: Tag[_hist_state] =
      Tag.materializeCStruct5Tag[Ptr[Ptr[HIST_ENTRY]], CInt, CInt, CInt, CInt]
    def apply()(using Zone): Ptr[_hist_state] =
      scala.scalanative.unsafe.alloc[_hist_state](1)
    def apply(
        entries: Ptr[Ptr[HIST_ENTRY]],
        offset: CInt,
        length: CInt,
        size: CInt,
        flags: CInt
    )(using Zone): Ptr[_hist_state] =
      val ____ptr = apply()
      (!____ptr).entries = entries
      (!____ptr).offset = offset
      (!____ptr).length = length
      (!____ptr).size = size
      (!____ptr).flags = flags
      ____ptr
    extension (struct: _hist_state)
      def entries: Ptr[Ptr[HIST_ENTRY]] = struct._1
      def entries_=(value: Ptr[Ptr[HIST_ENTRY]]): Unit = !struct.at1 = value
      def offset: CInt = struct._2
      def offset_=(value: CInt): Unit = !struct.at2 = value
      def length: CInt = struct._3
      def length_=(value: CInt): Unit = !struct.at3 = value
      def size: CInt = struct._4
      def size_=(value: CInt): Unit = !struct.at4 = value
      def flags: CInt = struct._5
      def flags_=(value: CInt): Unit = !struct.at5 = value

  /** [bindgen] header: readline.h
    */
  opaque type readline_state = CArray[CChar, Nat.Digit3[Nat._2, Nat._2, Nat._4]]
  object readline_state:
    given _tag: Tag[readline_state] =
      Tag.CArray[CChar, Nat.Digit3[Nat._2, Nat._2, Nat._4]](
        Tag.Byte,
        Tag.Digit3[Nat._2, Nat._2, Nat._4](Tag.Nat2, Tag.Nat2, Tag.Nat4)
      )
    def apply()(using Zone): Ptr[readline_state] =
      scala.scalanative.unsafe.alloc[readline_state](1)
    def apply(
        point: CInt,
        end: CInt,
        mark: CInt,
        buflen: CInt,
        buffer: CString,
        ul: Ptr[UNDO_LIST],
        prompt: CString,
        rlstate: CInt,
        done: CInt,
        kmap: Keymap,
        lastfunc: Ptr[rl_command_func_t],
        insmode: CInt,
        edmode: CInt,
        kseq: CString,
        kseqlen: CInt,
        pendingin: CInt,
        inf: Ptr[FILE],
        outf: Ptr[FILE],
        `macro`: CString,
        catchsigs: CInt,
        catchsigwinch: CInt,
        entryfunc: Ptr[rl_compentry_func_t],
        menuentryfunc: Ptr[rl_compentry_func_t],
        ignorefunc: Ptr[rl_compignore_func_t],
        attemptfunc: Ptr[rl_completion_func_t],
        wordbreakchars: CString,
        reserved: CArray[CChar, Nat.Digit2[Nat._6, Nat._4]]
    )(using Zone): Ptr[readline_state] =
      val ____ptr = apply()
      (!____ptr).point = point
      (!____ptr).end = end
      (!____ptr).mark = mark
      (!____ptr).buflen = buflen
      (!____ptr).buffer = buffer
      (!____ptr).ul = ul
      (!____ptr).prompt = prompt
      (!____ptr).rlstate = rlstate
      (!____ptr).done = done
      (!____ptr).kmap = kmap
      (!____ptr).lastfunc = lastfunc
      (!____ptr).insmode = insmode
      (!____ptr).edmode = edmode
      (!____ptr).kseq = kseq
      (!____ptr).kseqlen = kseqlen
      (!____ptr).pendingin = pendingin
      (!____ptr).inf = inf
      (!____ptr).outf = outf
      (!____ptr).`macro` = `macro`
      (!____ptr).catchsigs = catchsigs
      (!____ptr).catchsigwinch = catchsigwinch
      (!____ptr).entryfunc = entryfunc
      (!____ptr).menuentryfunc = menuentryfunc
      (!____ptr).ignorefunc = ignorefunc
      (!____ptr).attemptfunc = attemptfunc
      (!____ptr).wordbreakchars = wordbreakchars
      (!____ptr).reserved = reserved
      ____ptr
    extension (struct: readline_state)
      def point: CInt = !struct.at(offsets(0)).asInstanceOf[Ptr[CInt]]
      def point_=(value: CInt): Unit =
        !struct.at(offsets(0)).asInstanceOf[Ptr[CInt]] = value
      def end: CInt = !struct.at(offsets(1)).asInstanceOf[Ptr[CInt]]
      def end_=(value: CInt): Unit =
        !struct.at(offsets(1)).asInstanceOf[Ptr[CInt]] = value
      def mark: CInt = !struct.at(offsets(2)).asInstanceOf[Ptr[CInt]]
      def mark_=(value: CInt): Unit =
        !struct.at(offsets(2)).asInstanceOf[Ptr[CInt]] = value
      def buflen: CInt = !struct.at(offsets(3)).asInstanceOf[Ptr[CInt]]
      def buflen_=(value: CInt): Unit =
        !struct.at(offsets(3)).asInstanceOf[Ptr[CInt]] = value
      def buffer: CString = !struct.at(offsets(4)).asInstanceOf[Ptr[CString]]
      def buffer_=(value: CString): Unit =
        !struct.at(offsets(4)).asInstanceOf[Ptr[CString]] = value
      def ul: Ptr[UNDO_LIST] =
        !struct.at(offsets(5)).asInstanceOf[Ptr[Ptr[UNDO_LIST]]]
      def ul_=(value: Ptr[UNDO_LIST]): Unit =
        !struct.at(offsets(5)).asInstanceOf[Ptr[Ptr[UNDO_LIST]]] = value
      def prompt: CString = !struct.at(offsets(6)).asInstanceOf[Ptr[CString]]
      def prompt_=(value: CString): Unit =
        !struct.at(offsets(6)).asInstanceOf[Ptr[CString]] = value
      def rlstate: CInt = !struct.at(offsets(7)).asInstanceOf[Ptr[CInt]]
      def rlstate_=(value: CInt): Unit =
        !struct.at(offsets(7)).asInstanceOf[Ptr[CInt]] = value
      def done: CInt = !struct.at(offsets(8)).asInstanceOf[Ptr[CInt]]
      def done_=(value: CInt): Unit =
        !struct.at(offsets(8)).asInstanceOf[Ptr[CInt]] = value
      def kmap: Keymap = !struct.at(offsets(9)).asInstanceOf[Ptr[Keymap]]
      def kmap_=(value: Keymap): Unit =
        !struct.at(offsets(9)).asInstanceOf[Ptr[Keymap]] = value
      def lastfunc: Ptr[rl_command_func_t] =
        !struct.at(offsets(10)).asInstanceOf[Ptr[Ptr[rl_command_func_t]]]
      def lastfunc_=(value: Ptr[rl_command_func_t]): Unit =
        !struct.at(offsets(10)).asInstanceOf[Ptr[Ptr[rl_command_func_t]]] = value
      def insmode: CInt = !struct.at(offsets(11)).asInstanceOf[Ptr[CInt]]
      def insmode_=(value: CInt): Unit =
        !struct.at(offsets(11)).asInstanceOf[Ptr[CInt]] = value
      def edmode: CInt = !struct.at(offsets(12)).asInstanceOf[Ptr[CInt]]
      def edmode_=(value: CInt): Unit =
        !struct.at(offsets(12)).asInstanceOf[Ptr[CInt]] = value
      def kseq: CString = !struct.at(offsets(13)).asInstanceOf[Ptr[CString]]
      def kseq_=(value: CString): Unit =
        !struct.at(offsets(13)).asInstanceOf[Ptr[CString]] = value
      def kseqlen: CInt = !struct.at(offsets(14)).asInstanceOf[Ptr[CInt]]
      def kseqlen_=(value: CInt): Unit =
        !struct.at(offsets(14)).asInstanceOf[Ptr[CInt]] = value
      def pendingin: CInt = !struct.at(offsets(15)).asInstanceOf[Ptr[CInt]]
      def pendingin_=(value: CInt): Unit =
        !struct.at(offsets(15)).asInstanceOf[Ptr[CInt]] = value
      def inf: Ptr[FILE] = !struct.at(offsets(16)).asInstanceOf[Ptr[Ptr[FILE]]]
      def inf_=(value: Ptr[FILE]): Unit =
        !struct.at(offsets(16)).asInstanceOf[Ptr[Ptr[FILE]]] = value
      def outf: Ptr[FILE] = !struct.at(offsets(17)).asInstanceOf[Ptr[Ptr[FILE]]]
      def outf_=(value: Ptr[FILE]): Unit =
        !struct.at(offsets(17)).asInstanceOf[Ptr[Ptr[FILE]]] = value
      def `macro`: CString = !struct.at(offsets(18)).asInstanceOf[Ptr[CString]]
      def macro_=(value: CString): Unit =
        !struct.at(offsets(18)).asInstanceOf[Ptr[CString]] = value
      def catchsigs: CInt = !struct.at(offsets(19)).asInstanceOf[Ptr[CInt]]
      def catchsigs_=(value: CInt): Unit =
        !struct.at(offsets(19)).asInstanceOf[Ptr[CInt]] = value
      def catchsigwinch: CInt = !struct.at(offsets(20)).asInstanceOf[Ptr[CInt]]
      def catchsigwinch_=(value: CInt): Unit =
        !struct.at(offsets(20)).asInstanceOf[Ptr[CInt]] = value
      def entryfunc: Ptr[rl_compentry_func_t] =
        !struct.at(offsets(21)).asInstanceOf[Ptr[Ptr[rl_compentry_func_t]]]
      def entryfunc_=(value: Ptr[rl_compentry_func_t]): Unit =
        !struct.at(offsets(21)).asInstanceOf[Ptr[Ptr[rl_compentry_func_t]]] = value
      def menuentryfunc: Ptr[rl_compentry_func_t] =
        !struct.at(offsets(22)).asInstanceOf[Ptr[Ptr[rl_compentry_func_t]]]
      def menuentryfunc_=(value: Ptr[rl_compentry_func_t]): Unit =
        !struct.at(offsets(22)).asInstanceOf[Ptr[Ptr[rl_compentry_func_t]]] = value
      def ignorefunc: Ptr[rl_compignore_func_t] =
        !struct.at(offsets(23)).asInstanceOf[Ptr[Ptr[rl_compignore_func_t]]]
      def ignorefunc_=(value: Ptr[rl_compignore_func_t]): Unit =
        !struct.at(offsets(23)).asInstanceOf[Ptr[Ptr[rl_compignore_func_t]]] = value
      def attemptfunc: Ptr[rl_completion_func_t] =
        !struct.at(offsets(24)).asInstanceOf[Ptr[Ptr[rl_completion_func_t]]]
      def attemptfunc_=(value: Ptr[rl_completion_func_t]): Unit =
        !struct.at(offsets(24)).asInstanceOf[Ptr[Ptr[rl_completion_func_t]]] = value
      def wordbreakchars: CString =
        !struct.at(offsets(25)).asInstanceOf[Ptr[CString]]
      def wordbreakchars_=(value: CString): Unit =
        !struct.at(offsets(25)).asInstanceOf[Ptr[CString]] = value
      def reserved: CArray[CChar, Nat.Digit2[Nat._6, Nat._4]] = !struct
        .at(offsets(26))
        .asInstanceOf[Ptr[CArray[CChar, Nat.Digit2[Nat._6, Nat._4]]]]
      def reserved_=(value: CArray[CChar, Nat.Digit2[Nat._6, Nat._4]]): Unit =
        !struct
          .at(offsets(26))
          .asInstanceOf[Ptr[CArray[CChar, Nat.Digit2[Nat._6, Nat._4]]]] = value
    val offsets: Array[Int] =
      val res = Array.ofDim[Int](27)
      def align(offset: Int, alignment: Int) = {
        val alignmentMask = alignment - 1
        val padding =
          if ((offset & alignmentMask) == 0) 0
          else alignment - (offset & alignmentMask)
        offset + padding
      }

      res(0) = align(0, alignmentof[CInt].toInt)
      res(1) = align(res(0) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(2) = align(res(1) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(3) = align(res(2) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(4) = align(res(3) + sizeof[CInt].toInt, alignmentof[CString].toInt)
      res(5) = align(res(4) + sizeof[CString].toInt, alignmentof[Ptr[UNDO_LIST]].toInt)
      res(6) = align(res(5) + sizeof[Ptr[UNDO_LIST]].toInt, alignmentof[CString].toInt)
      res(7) = align(res(6) + sizeof[CString].toInt, alignmentof[CInt].toInt)
      res(8) = align(res(7) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(9) = align(res(8) + sizeof[CInt].toInt, alignmentof[Keymap].toInt)
      res(10) = align(
        res(9) + sizeof[Keymap].toInt,
        alignmentof[Ptr[rl_command_func_t]].toInt
      )
      res(11) = align(
        res(10) + sizeof[Ptr[rl_command_func_t]].toInt,
        alignmentof[CInt].toInt
      )
      res(12) = align(res(11) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(13) = align(res(12) + sizeof[CInt].toInt, alignmentof[CString].toInt)
      res(14) = align(res(13) + sizeof[CString].toInt, alignmentof[CInt].toInt)
      res(15) = align(res(14) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(16) = align(res(15) + sizeof[CInt].toInt, alignmentof[Ptr[FILE]].toInt)
      res(17) = align(res(16) + sizeof[Ptr[FILE]].toInt, alignmentof[Ptr[FILE]].toInt)
      res(18) = align(res(17) + sizeof[Ptr[FILE]].toInt, alignmentof[CString].toInt)
      res(19) = align(res(18) + sizeof[CString].toInt, alignmentof[CInt].toInt)
      res(20) = align(res(19) + sizeof[CInt].toInt, alignmentof[CInt].toInt)
      res(21) = align(
        res(20) + sizeof[CInt].toInt,
        alignmentof[Ptr[rl_compentry_func_t]].toInt
      )
      res(22) = align(
        res(21) + sizeof[Ptr[rl_compentry_func_t]].toInt,
        alignmentof[Ptr[rl_compentry_func_t]].toInt
      )
      res(23) = align(
        res(22) + sizeof[Ptr[rl_compentry_func_t]].toInt,
        alignmentof[Ptr[rl_compignore_func_t]].toInt
      )
      res(24) = align(
        res(23) + sizeof[Ptr[rl_compignore_func_t]].toInt,
        alignmentof[Ptr[rl_completion_func_t]].toInt
      )
      res(25) = align(
        res(24) + sizeof[Ptr[rl_completion_func_t]].toInt,
        alignmentof[CString].toInt
      )
      res(26) = align(res(25) + sizeof[CString].toInt, alignmentof[CChar].toInt)
      res
    end offsets

  /** [bindgen] header: readline.h
    */
  opaque type undo_list = CStruct5[Ptr[Byte], CInt, CInt, CString, undo_code]
  object undo_list:
    given _tag: Tag[undo_list] =
      Tag.materializeCStruct5Tag[Ptr[Byte], CInt, CInt, CString, undo_code]
    def apply()(using Zone): Ptr[undo_list] =
      scala.scalanative.unsafe.alloc[undo_list](1)
    def apply(
        next: Ptr[undo_list],
        start: CInt,
        end: CInt,
        text: CString,
        what: undo_code
    )(using Zone): Ptr[undo_list] =
      val ____ptr = apply()
      (!____ptr).next = next
      (!____ptr).start = start
      (!____ptr).end = end
      (!____ptr).text = text
      (!____ptr).what = what
      ____ptr
    extension (struct: undo_list)
      def next: Ptr[undo_list] = struct._1.asInstanceOf[Ptr[undo_list]]
      def next_=(value: Ptr[undo_list]): Unit = !struct.at1 = value.asInstanceOf[Ptr[Byte]]
      def start: CInt = struct._2
      def start_=(value: CInt): Unit = !struct.at2 = value
      def end: CInt = struct._3
      def end_=(value: CInt): Unit = !struct.at3 = value
      def text: CString = struct._4
      def text_=(value: CString): Unit = !struct.at4 = value
      def what: undo_code = struct._5
      def what_=(value: undo_code): Unit = !struct.at5 = value

@extern
private[readline] object extern_functions:
  import _root_.readline.aliases.*
  import _root_.readline.enumerations.*
  import _root_.readline.structs.*

  /** [bindgen] header: readline.h
    */
  def add_history(_0: CString): Unit = extern

  /** [bindgen] header: readline.h
    */
  def add_history_time(_0: CString): Unit = extern

  /** [bindgen] header: readline.h
    */
  def alloc_history_entry(_0: CString, _1: CString): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def append_history(_0: CInt, _1: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def clear_history(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def copy_history_entry(_0: Ptr[HIST_ENTRY]): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def current_history(): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def free_history_entry(_0: Ptr[HIST_ENTRY]): histdata_t = extern

  /** [bindgen] header: readline.h
    */
  def get_history_event(_0: CString, _1: Ptr[CInt], _2: CInt): CString = extern

  /** [bindgen] header: readline.h
    */
  def history_arg_extract(_0: CInt, _1: CInt, _2: CString): CString = extern

  /** [bindgen] header: readline.h
    */
  def history_expand(_0: CString, _1: Ptr[CString]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_get(_0: CInt): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def history_get_history_state(): Ptr[HISTORY_STATE] = extern

  /** [bindgen] header: readline.h
    */
  def history_get_time(_0: Ptr[HIST_ENTRY]): time_t = extern

  /** [bindgen] header: readline.h
    */
  def history_is_stifled(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_list(): Ptr[Ptr[HIST_ENTRY]] = extern

  /** [bindgen] header: readline.h
    */
  def history_search(_0: CString, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_search_pos(_0: CString, _1: CInt, _2: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_search_prefix(_0: CString, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_set_history_state(_0: Ptr[HISTORY_STATE]): Unit = extern

  /** [bindgen] header: readline.h
    */
  def history_set_pos(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_tokenize(_0: CString): Ptr[CString] = extern

  /** [bindgen] header: readline.h
    */
  def history_total_bytes(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def history_truncate_file(_0: CString, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def next_history(): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def previous_history(): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def read_history(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def read_history_range(_0: CString, _1: CInt, _2: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def readline(_0: CString): CString = extern

  /** [bindgen] header: readline.h
    */
  def remove_history(_0: CInt): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def remove_history_range(_0: CInt, _1: CInt): Ptr[Ptr[HIST_ENTRY]] = extern

  /** [bindgen] header: readline.h
    */
  def replace_history_entry(
      _0: CInt,
      _1: CString,
      _2: histdata_t
  ): Ptr[HIST_ENTRY] = extern

  /** [bindgen] header: readline.h
    */
  def rl_abort(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_activate_mark(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_add_defun(_0: CString, _1: Ptr[rl_command_func_t], _2: CInt): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_add_funmap_entry(_0: CString, _1: Ptr[rl_command_func_t]): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_add_undo(_0: undo_code, _1: CInt, _2: CInt, _3: CString): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_alphabetic(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_arrow_keys(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_byte(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_char(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_char_search(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_kill_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_kill_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_menu_complete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_backward_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_beg_of_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_begin_undo_group(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_beginning_of_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_key(_0: CInt, _1: Ptr[rl_command_func_t]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_key_if_unbound(_0: CInt, _1: Ptr[rl_command_func_t]): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_key_if_unbound_in_map(
      _0: CInt,
      _1: Ptr[rl_command_func_t],
      _2: Keymap
  ): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_key_in_map(
      _0: CInt,
      _1: Ptr[rl_command_func_t],
      _2: Keymap
  ): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_keyseq(_0: CString, _1: Ptr[rl_command_func_t]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_keyseq_if_unbound(_0: CString, _1: Ptr[rl_command_func_t]): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_keyseq_if_unbound_in_map(
      _0: CString,
      _1: Ptr[rl_command_func_t],
      _2: Keymap
  ): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bind_keyseq_in_map(
      _0: CString,
      _1: Ptr[rl_command_func_t],
      _2: Keymap
  ): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_bracketed_paste_begin(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_call_last_kbd_macro(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_callback_handler_install(_0: CString, _1: Ptr[rl_vcpfunc_t]): Unit =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_callback_handler_remove(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_callback_read_char(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_callback_sigcleanup(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_capitalize_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_char_search(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_character_len(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_check_signals(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_cleanup_after_signal(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_display(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_history(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_message(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_pending_input(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_screen(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_signals(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_clear_visible_line(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_complete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_complete_internal(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_completion_matches(
      _0: CString,
      _1: Ptr[rl_compentry_func_t]
  ): Ptr[CString] = extern

  /** [bindgen] header: readline.h
    */
  def rl_completion_mode(_0: Ptr[rl_command_func_t]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_copy_backward_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_copy_forward_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_copy_keymap(_0: Keymap): Keymap = extern

  /** [bindgen] header: readline.h
    */
  def rl_copy_region_to_kill(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_copy_text(_0: CInt, _1: CInt): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_crlf(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_deactivate_mark(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_delete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_delete_horizontal_space(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_delete_or_show_completions(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_delete_text(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_deprep_terminal(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_digit_argument(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_ding(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_discard_argument(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_discard_keymap(_0: Keymap): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_display_match_list(_0: Ptr[CString], _1: CInt, _2: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_do_lowercase_version(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_do_undo(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_downcase_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_dump_functions(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_dump_macros(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_dump_variables(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_echo_signal_char(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_emacs_editing_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_empty_keymap(_0: Keymap): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_end_kbd_macro(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_end_of_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_end_of_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_end_undo_group(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_exchange_point_and_mark(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_execute_next(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_expand_prompt(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_extend_line_buffer(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_fetch_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_filename_completion_function(_0: CString, _1: CInt): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_forced_update_display(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_forward(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_forward_byte(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_forward_char(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_forward_search_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_forward_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_free(_0: Ptr[Byte]): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_free_keymap(_0: Keymap): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_free_line_state(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_free_undo_list(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_function_dumper(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_function_of_keyseq(
      _0: CString,
      _1: Keymap,
      _2: Ptr[CInt]
  ): Ptr[rl_command_func_t] = extern

  /** [bindgen] header: readline.h
    */
  def rl_function_of_keyseq_len(
      _0: CString,
      _1: size_t,
      _2: Keymap,
      _3: Ptr[CInt]
  ): Ptr[rl_command_func_t] = extern

  /** [bindgen] header: readline.h
    */
  def rl_funmap_names(): Ptr[CString] = extern

  /** [bindgen] header: readline.h
    */
  def rl_generic_bind(_0: CInt, _1: CString, _2: CString, _3: Keymap): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_get_keymap(): Keymap = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_keymap_by_name(_0: CString): Keymap = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_keymap_name(_0: Keymap): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_keymap_name_from_edit_mode(): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_next_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_previous_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_screen_size(_0: Ptr[CInt], _1: Ptr[CInt]): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_get_termcap(_0: CString): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_getc(_0: Ptr[FILE]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_history_search_backward(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_history_search_forward(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_history_substr_search_backward(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_history_substr_search_forward(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_initialize(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_initialize_funmap(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_insert(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_insert_close(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_insert_comment(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_insert_completions(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_insert_text(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_invoking_keyseqs(_0: Ptr[rl_command_func_t]): Ptr[CString] = extern

  /** [bindgen] header: readline.h
    */
  def rl_invoking_keyseqs_in_map(
      _0: Ptr[rl_command_func_t],
      _1: Keymap
  ): Ptr[CString] = extern

  /** [bindgen] header: readline.h
    */
  def rl_keep_mark_active(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_kill_full_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_kill_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_kill_region(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_kill_text(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_kill_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_list_funmap_names(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_macro_bind(_0: CString, _1: CString, _2: Keymap): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_macro_dumper(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_make_bare_keymap(): Keymap = extern

  /** [bindgen] header: readline.h
    */
  def rl_make_keymap(): Keymap = extern

  /** [bindgen] header: readline.h
    */
  def rl_mark_active_p(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_maybe_replace_line(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_maybe_save_line(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_maybe_unsave_line(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_menu_complete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_message(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_modifying(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_named_function(_0: CString): Ptr[rl_command_func_t] = extern

  /** [bindgen] header: readline.h
    */
  def rl_newline(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_next_screen_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_noninc_forward_search(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_noninc_forward_search_again(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_noninc_reverse_search(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_noninc_reverse_search_again(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_old_menu_complete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_on_new_line(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_on_new_line_with_prompt(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_operate_and_get_next(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_overwrite_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_parse_and_bind(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_pending_signal(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_possible_completions(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_prep_terminal(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_previous_screen_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_print_last_kbd_macro(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_push_macro_input(_0: CString): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_quoted_insert(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_re_read_init_file(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_read_init_file(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_read_key(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_redisplay(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_redraw_prompt_last_line(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_refresh_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_replace_line(_0: CString, _1: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_reset_after_signal(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_reset_line_state(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_reset_screen_size(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_reset_terminal(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_resize_terminal(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_restart_output(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_restore_prompt(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_restore_state(_0: Ptr[readline_state]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_reverse_search_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_revert_line(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_rubout(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_rubout_or_delete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_save_prompt(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_save_state(_0: Ptr[readline_state]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_key(_0: CString, _1: Ptr[rl_command_func_t], _2: Keymap): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_set_keyboard_input_timeout(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_keymap(_0: Keymap): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_keymap_from_edit_mode(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_keymap_name(_0: CString, _1: Keymap): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_mark(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_paren_blink_timeout(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_prompt(_0: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_screen_size(_0: CInt, _1: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_signals(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_set_timeout(_0: CUnsignedInt, _1: CUnsignedInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_show_char(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_skip_csi_sequence(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_start_kbd_macro(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_stop_output(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_stuff_char(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_tab_insert(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_tilde_expand(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_timeout_remaining(_0: Ptr[CUnsignedInt], _1: Ptr[CUnsignedInt]): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_translate_keyseq(_0: CString, _1: CString, _2: Ptr[CInt]): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_transpose_chars(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_transpose_words(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_trim_arg_from_keyseq(_0: CString, _1: size_t, _2: Keymap): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_tty_set_default_bindings(_0: Keymap): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_tty_set_echoing(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_tty_status(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_tty_unset_default_bindings(_0: Keymap): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_unbind_command_in_map(_0: CString, _1: Keymap): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_unbind_function_in_map(_0: Ptr[rl_command_func_t], _1: Keymap): CInt =
    extern

  /** [bindgen] header: readline.h
    */
  def rl_unbind_key(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_unbind_key_in_map(_0: CInt, _1: Keymap): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_undo_command(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_universal_argument(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_unix_filename_rubout(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_unix_line_discard(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_unix_word_rubout(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_untranslate_keyseq(_0: CInt): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_upcase_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_username_completion_function(_0: CString, _1: CInt): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_variable_bind(_0: CString, _1: CString): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_variable_dumper(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_variable_value(_0: CString): CString = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_append_eol(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_append_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_arg_digit(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_bWord(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_back_to_indent(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_bracktype(_0: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_bword(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_change_case(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_change_char(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_change_to(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_char_search(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_check(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_column(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_complete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_delete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_delete_to(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_domove(_0: CInt, _1: Ptr[CInt]): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_eWord(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_editing_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_end_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_eof_maybe(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_eword(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_fWord(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_fetch_history(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_first_print(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_fword(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_goto_mark(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_insert_beg(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_insert_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_insertion_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_match(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_movement_mode(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_next_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_overstrike(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_overstrike_delete(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_prev_word(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_put(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_redo(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_replace(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_rubout(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_search(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_search_again(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_set_mark(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_start_inserting(_0: CInt, _1: CInt, _2: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_subst(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_tilde_expand(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_undo(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_unix_word_rubout(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_yank_arg(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_yank_pop(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_vi_yank_to(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_yank(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_yank_last_arg(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_yank_nth_arg(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def rl_yank_pop(_0: CInt, _1: CInt): CInt = extern

  /** [bindgen] header: readline.h
    */
  def stifle_history(_0: CInt): Unit = extern

  /** [bindgen] header: readline.h
    */
  def unstifle_history(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def using_history(): Unit = extern

  /** [bindgen] header: readline.h
    */
  def where_history(): CInt = extern

  /** [bindgen] header: readline.h
    */
  def write_history(_0: CString): CInt = extern

object functions:
  import _root_.readline.aliases.*
  import _root_.readline.enumerations.*
  import _root_.readline.structs.*
  export extern_functions.*

object types:
  export _root_.readline.structs.*
  export _root_.readline.aliases.*
  export _root_.readline.enumerations.*

object all:
  export _root_.readline.enumerations.undo_code
  export _root_.readline.aliases.FILE
  export _root_.readline.aliases.Keymap
  export _root_.readline.aliases.histdata_t
  export _root_.readline.aliases.rl_command_func_t
  export _root_.readline.aliases.rl_compentry_func_t
  export _root_.readline.aliases.rl_compignore_func_t
  export _root_.readline.aliases.rl_completion_func_t
  export _root_.readline.aliases.rl_vcpfunc_t
  export _root_.readline.aliases.size_t
  export _root_.readline.aliases.time_t
  export _root_.readline.structs.FUNMAP
  export _root_.readline.structs.HISTORY_STATE
  export _root_.readline.structs.HIST_ENTRY
  export _root_.readline.structs.KEYMAP_ENTRY
  export _root_.readline.structs.UNDO_LIST
  export _root_.readline.structs._funmap
  export _root_.readline.structs._hist_entry
  export _root_.readline.structs._hist_state
  export _root_.readline.structs.readline_state
  export _root_.readline.structs.undo_list
  export _root_.readline.functions.add_history
  export _root_.readline.functions.add_history_time
  export _root_.readline.functions.alloc_history_entry
  export _root_.readline.functions.append_history
  export _root_.readline.functions.clear_history
  export _root_.readline.functions.copy_history_entry
  export _root_.readline.functions.current_history
  export _root_.readline.functions.free_history_entry
  export _root_.readline.functions.get_history_event
  export _root_.readline.functions.history_arg_extract
  export _root_.readline.functions.history_expand
  export _root_.readline.functions.history_get
  export _root_.readline.functions.history_get_history_state
  export _root_.readline.functions.history_get_time
  export _root_.readline.functions.history_is_stifled
  export _root_.readline.functions.history_list
  export _root_.readline.functions.history_search
  export _root_.readline.functions.history_search_pos
  export _root_.readline.functions.history_search_prefix
  export _root_.readline.functions.history_set_history_state
  export _root_.readline.functions.history_set_pos
  export _root_.readline.functions.history_tokenize
  export _root_.readline.functions.history_total_bytes
  export _root_.readline.functions.history_truncate_file
  export _root_.readline.functions.next_history
  export _root_.readline.functions.previous_history
  export _root_.readline.functions.read_history
  export _root_.readline.functions.read_history_range
  export _root_.readline.functions.readline
  export _root_.readline.functions.remove_history
  export _root_.readline.functions.remove_history_range
  export _root_.readline.functions.replace_history_entry
  export _root_.readline.functions.rl_abort
  export _root_.readline.functions.rl_activate_mark
  export _root_.readline.functions.rl_add_defun
  export _root_.readline.functions.rl_add_funmap_entry
  export _root_.readline.functions.rl_add_undo
  export _root_.readline.functions.rl_alphabetic
  export _root_.readline.functions.rl_arrow_keys
  export _root_.readline.functions.rl_backward
  export _root_.readline.functions.rl_backward_byte
  export _root_.readline.functions.rl_backward_char
  export _root_.readline.functions.rl_backward_char_search
  export _root_.readline.functions.rl_backward_kill_line
  export _root_.readline.functions.rl_backward_kill_word
  export _root_.readline.functions.rl_backward_menu_complete
  export _root_.readline.functions.rl_backward_word
  export _root_.readline.functions.rl_beg_of_line
  export _root_.readline.functions.rl_begin_undo_group
  export _root_.readline.functions.rl_beginning_of_history
  export _root_.readline.functions.rl_bind_key
  export _root_.readline.functions.rl_bind_key_if_unbound
  export _root_.readline.functions.rl_bind_key_if_unbound_in_map
  export _root_.readline.functions.rl_bind_key_in_map
  export _root_.readline.functions.rl_bind_keyseq
  export _root_.readline.functions.rl_bind_keyseq_if_unbound
  export _root_.readline.functions.rl_bind_keyseq_if_unbound_in_map
  export _root_.readline.functions.rl_bind_keyseq_in_map
  export _root_.readline.functions.rl_bracketed_paste_begin
  export _root_.readline.functions.rl_call_last_kbd_macro
  export _root_.readline.functions.rl_callback_handler_install
  export _root_.readline.functions.rl_callback_handler_remove
  export _root_.readline.functions.rl_callback_read_char
  export _root_.readline.functions.rl_callback_sigcleanup
  export _root_.readline.functions.rl_capitalize_word
  export _root_.readline.functions.rl_char_search
  export _root_.readline.functions.rl_character_len
  export _root_.readline.functions.rl_check_signals
  export _root_.readline.functions.rl_cleanup_after_signal
  export _root_.readline.functions.rl_clear_display
  export _root_.readline.functions.rl_clear_history
  export _root_.readline.functions.rl_clear_message
  export _root_.readline.functions.rl_clear_pending_input
  export _root_.readline.functions.rl_clear_screen
  export _root_.readline.functions.rl_clear_signals
  export _root_.readline.functions.rl_clear_visible_line
  export _root_.readline.functions.rl_complete
  export _root_.readline.functions.rl_complete_internal
  export _root_.readline.functions.rl_completion_matches
  export _root_.readline.functions.rl_completion_mode
  export _root_.readline.functions.rl_copy_backward_word
  export _root_.readline.functions.rl_copy_forward_word
  export _root_.readline.functions.rl_copy_keymap
  export _root_.readline.functions.rl_copy_region_to_kill
  export _root_.readline.functions.rl_copy_text
  export _root_.readline.functions.rl_crlf
  export _root_.readline.functions.rl_deactivate_mark
  export _root_.readline.functions.rl_delete
  export _root_.readline.functions.rl_delete_horizontal_space
  export _root_.readline.functions.rl_delete_or_show_completions
  export _root_.readline.functions.rl_delete_text
  export _root_.readline.functions.rl_deprep_terminal
  export _root_.readline.functions.rl_digit_argument
  export _root_.readline.functions.rl_ding
  export _root_.readline.functions.rl_discard_argument
  export _root_.readline.functions.rl_discard_keymap
  export _root_.readline.functions.rl_display_match_list
  export _root_.readline.functions.rl_do_lowercase_version
  export _root_.readline.functions.rl_do_undo
  export _root_.readline.functions.rl_downcase_word
  export _root_.readline.functions.rl_dump_functions
  export _root_.readline.functions.rl_dump_macros
  export _root_.readline.functions.rl_dump_variables
  export _root_.readline.functions.rl_echo_signal_char
  export _root_.readline.functions.rl_emacs_editing_mode
  export _root_.readline.functions.rl_empty_keymap
  export _root_.readline.functions.rl_end_kbd_macro
  export _root_.readline.functions.rl_end_of_history
  export _root_.readline.functions.rl_end_of_line
  export _root_.readline.functions.rl_end_undo_group
  export _root_.readline.functions.rl_exchange_point_and_mark
  export _root_.readline.functions.rl_execute_next
  export _root_.readline.functions.rl_expand_prompt
  export _root_.readline.functions.rl_extend_line_buffer
  export _root_.readline.functions.rl_fetch_history
  export _root_.readline.functions.rl_filename_completion_function
  export _root_.readline.functions.rl_forced_update_display
  export _root_.readline.functions.rl_forward
  export _root_.readline.functions.rl_forward_byte
  export _root_.readline.functions.rl_forward_char
  export _root_.readline.functions.rl_forward_search_history
  export _root_.readline.functions.rl_forward_word
  export _root_.readline.functions.rl_free
  export _root_.readline.functions.rl_free_keymap
  export _root_.readline.functions.rl_free_line_state
  export _root_.readline.functions.rl_free_undo_list
  export _root_.readline.functions.rl_function_dumper
  export _root_.readline.functions.rl_function_of_keyseq
  export _root_.readline.functions.rl_function_of_keyseq_len
  export _root_.readline.functions.rl_funmap_names
  export _root_.readline.functions.rl_generic_bind
  export _root_.readline.functions.rl_get_keymap
  export _root_.readline.functions.rl_get_keymap_by_name
  export _root_.readline.functions.rl_get_keymap_name
  export _root_.readline.functions.rl_get_keymap_name_from_edit_mode
  export _root_.readline.functions.rl_get_next_history
  export _root_.readline.functions.rl_get_previous_history
  export _root_.readline.functions.rl_get_screen_size
  export _root_.readline.functions.rl_get_termcap
  export _root_.readline.functions.rl_getc
  export _root_.readline.functions.rl_history_search_backward
  export _root_.readline.functions.rl_history_search_forward
  export _root_.readline.functions.rl_history_substr_search_backward
  export _root_.readline.functions.rl_history_substr_search_forward
  export _root_.readline.functions.rl_initialize
  export _root_.readline.functions.rl_initialize_funmap
  export _root_.readline.functions.rl_insert
  export _root_.readline.functions.rl_insert_close
  export _root_.readline.functions.rl_insert_comment
  export _root_.readline.functions.rl_insert_completions
  export _root_.readline.functions.rl_insert_text
  export _root_.readline.functions.rl_invoking_keyseqs
  export _root_.readline.functions.rl_invoking_keyseqs_in_map
  export _root_.readline.functions.rl_keep_mark_active
  export _root_.readline.functions.rl_kill_full_line
  export _root_.readline.functions.rl_kill_line
  export _root_.readline.functions.rl_kill_region
  export _root_.readline.functions.rl_kill_text
  export _root_.readline.functions.rl_kill_word
  export _root_.readline.functions.rl_list_funmap_names
  export _root_.readline.functions.rl_macro_bind
  export _root_.readline.functions.rl_macro_dumper
  export _root_.readline.functions.rl_make_bare_keymap
  export _root_.readline.functions.rl_make_keymap
  export _root_.readline.functions.rl_mark_active_p
  export _root_.readline.functions.rl_maybe_replace_line
  export _root_.readline.functions.rl_maybe_save_line
  export _root_.readline.functions.rl_maybe_unsave_line
  export _root_.readline.functions.rl_menu_complete
  export _root_.readline.functions.rl_message
  export _root_.readline.functions.rl_modifying
  export _root_.readline.functions.rl_named_function
  export _root_.readline.functions.rl_newline
  export _root_.readline.functions.rl_next_screen_line
  export _root_.readline.functions.rl_noninc_forward_search
  export _root_.readline.functions.rl_noninc_forward_search_again
  export _root_.readline.functions.rl_noninc_reverse_search
  export _root_.readline.functions.rl_noninc_reverse_search_again
  export _root_.readline.functions.rl_old_menu_complete
  export _root_.readline.functions.rl_on_new_line
  export _root_.readline.functions.rl_on_new_line_with_prompt
  export _root_.readline.functions.rl_operate_and_get_next
  export _root_.readline.functions.rl_overwrite_mode
  export _root_.readline.functions.rl_parse_and_bind
  export _root_.readline.functions.rl_pending_signal
  export _root_.readline.functions.rl_possible_completions
  export _root_.readline.functions.rl_prep_terminal
  export _root_.readline.functions.rl_previous_screen_line
  export _root_.readline.functions.rl_print_last_kbd_macro
  export _root_.readline.functions.rl_push_macro_input
  export _root_.readline.functions.rl_quoted_insert
  export _root_.readline.functions.rl_re_read_init_file
  export _root_.readline.functions.rl_read_init_file
  export _root_.readline.functions.rl_read_key
  export _root_.readline.functions.rl_redisplay
  export _root_.readline.functions.rl_redraw_prompt_last_line
  export _root_.readline.functions.rl_refresh_line
  export _root_.readline.functions.rl_replace_line
  export _root_.readline.functions.rl_reset_after_signal
  export _root_.readline.functions.rl_reset_line_state
  export _root_.readline.functions.rl_reset_screen_size
  export _root_.readline.functions.rl_reset_terminal
  export _root_.readline.functions.rl_resize_terminal
  export _root_.readline.functions.rl_restart_output
  export _root_.readline.functions.rl_restore_prompt
  export _root_.readline.functions.rl_restore_state
  export _root_.readline.functions.rl_reverse_search_history
  export _root_.readline.functions.rl_revert_line
  export _root_.readline.functions.rl_rubout
  export _root_.readline.functions.rl_rubout_or_delete
  export _root_.readline.functions.rl_save_prompt
  export _root_.readline.functions.rl_save_state
  export _root_.readline.functions.rl_set_key
  export _root_.readline.functions.rl_set_keyboard_input_timeout
  export _root_.readline.functions.rl_set_keymap
  export _root_.readline.functions.rl_set_keymap_from_edit_mode
  export _root_.readline.functions.rl_set_keymap_name
  export _root_.readline.functions.rl_set_mark
  export _root_.readline.functions.rl_set_paren_blink_timeout
  export _root_.readline.functions.rl_set_prompt
  export _root_.readline.functions.rl_set_screen_size
  export _root_.readline.functions.rl_set_signals
  export _root_.readline.functions.rl_set_timeout
  export _root_.readline.functions.rl_show_char
  export _root_.readline.functions.rl_skip_csi_sequence
  export _root_.readline.functions.rl_start_kbd_macro
  export _root_.readline.functions.rl_stop_output
  export _root_.readline.functions.rl_stuff_char
  export _root_.readline.functions.rl_tab_insert
  export _root_.readline.functions.rl_tilde_expand
  export _root_.readline.functions.rl_timeout_remaining
  export _root_.readline.functions.rl_translate_keyseq
  export _root_.readline.functions.rl_transpose_chars
  export _root_.readline.functions.rl_transpose_words
  export _root_.readline.functions.rl_trim_arg_from_keyseq
  export _root_.readline.functions.rl_tty_set_default_bindings
  export _root_.readline.functions.rl_tty_set_echoing
  export _root_.readline.functions.rl_tty_status
  export _root_.readline.functions.rl_tty_unset_default_bindings
  export _root_.readline.functions.rl_unbind_command_in_map
  export _root_.readline.functions.rl_unbind_function_in_map
  export _root_.readline.functions.rl_unbind_key
  export _root_.readline.functions.rl_unbind_key_in_map
  export _root_.readline.functions.rl_undo_command
  export _root_.readline.functions.rl_universal_argument
  export _root_.readline.functions.rl_unix_filename_rubout
  export _root_.readline.functions.rl_unix_line_discard
  export _root_.readline.functions.rl_unix_word_rubout
  export _root_.readline.functions.rl_untranslate_keyseq
  export _root_.readline.functions.rl_upcase_word
  export _root_.readline.functions.rl_username_completion_function
  export _root_.readline.functions.rl_variable_bind
  export _root_.readline.functions.rl_variable_dumper
  export _root_.readline.functions.rl_variable_value
  export _root_.readline.functions.rl_vi_append_eol
  export _root_.readline.functions.rl_vi_append_mode
  export _root_.readline.functions.rl_vi_arg_digit
  export _root_.readline.functions.rl_vi_bWord
  export _root_.readline.functions.rl_vi_back_to_indent
  export _root_.readline.functions.rl_vi_bracktype
  export _root_.readline.functions.rl_vi_bword
  export _root_.readline.functions.rl_vi_change_case
  export _root_.readline.functions.rl_vi_change_char
  export _root_.readline.functions.rl_vi_change_to
  export _root_.readline.functions.rl_vi_char_search
  export _root_.readline.functions.rl_vi_check
  export _root_.readline.functions.rl_vi_column
  export _root_.readline.functions.rl_vi_complete
  export _root_.readline.functions.rl_vi_delete
  export _root_.readline.functions.rl_vi_delete_to
  export _root_.readline.functions.rl_vi_domove
  export _root_.readline.functions.rl_vi_eWord
  export _root_.readline.functions.rl_vi_editing_mode
  export _root_.readline.functions.rl_vi_end_word
  export _root_.readline.functions.rl_vi_eof_maybe
  export _root_.readline.functions.rl_vi_eword
  export _root_.readline.functions.rl_vi_fWord
  export _root_.readline.functions.rl_vi_fetch_history
  export _root_.readline.functions.rl_vi_first_print
  export _root_.readline.functions.rl_vi_fword
  export _root_.readline.functions.rl_vi_goto_mark
  export _root_.readline.functions.rl_vi_insert_beg
  export _root_.readline.functions.rl_vi_insert_mode
  export _root_.readline.functions.rl_vi_insertion_mode
  export _root_.readline.functions.rl_vi_match
  export _root_.readline.functions.rl_vi_movement_mode
  export _root_.readline.functions.rl_vi_next_word
  export _root_.readline.functions.rl_vi_overstrike
  export _root_.readline.functions.rl_vi_overstrike_delete
  export _root_.readline.functions.rl_vi_prev_word
  export _root_.readline.functions.rl_vi_put
  export _root_.readline.functions.rl_vi_redo
  export _root_.readline.functions.rl_vi_replace
  export _root_.readline.functions.rl_vi_rubout
  export _root_.readline.functions.rl_vi_search
  export _root_.readline.functions.rl_vi_search_again
  export _root_.readline.functions.rl_vi_set_mark
  export _root_.readline.functions.rl_vi_start_inserting
  export _root_.readline.functions.rl_vi_subst
  export _root_.readline.functions.rl_vi_tilde_expand
  export _root_.readline.functions.rl_vi_undo
  export _root_.readline.functions.rl_vi_unix_word_rubout
  export _root_.readline.functions.rl_vi_yank_arg
  export _root_.readline.functions.rl_vi_yank_pop
  export _root_.readline.functions.rl_vi_yank_to
  export _root_.readline.functions.rl_yank
  export _root_.readline.functions.rl_yank_last_arg
  export _root_.readline.functions.rl_yank_nth_arg
  export _root_.readline.functions.rl_yank_pop
  export _root_.readline.functions.stifle_history
  export _root_.readline.functions.unstifle_history
  export _root_.readline.functions.using_history
  export _root_.readline.functions.where_history
  export _root_.readline.functions.write_history
