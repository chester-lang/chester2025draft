package linenoise // sn-bindgen-x86_64-pc-linux --package linenoise --header linenoise.h --scala > ~/Downloads/linenoise.scala

import _root_.scala.scalanative.*
import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*

object aliases:
  import _root_.linenoise.structs.*

  /** [bindgen] header: linenoise.h
    */
  type linenoiseCompletionCallback =
    CFuncPtr3[CString, Ptr[linenoiseCompletions], Ptr[Byte], Unit]
  object linenoiseCompletionCallback:
    given _tag: Tag[linenoiseCompletionCallback] = Tag
      .materializeCFuncPtr3[CString, Ptr[linenoiseCompletions], Ptr[Byte], Unit]
    inline def apply(
        inline o: CFuncPtr3[CString, Ptr[linenoiseCompletions], Ptr[Byte], Unit]
    ): linenoiseCompletionCallback = o
    extension (v: linenoiseCompletionCallback) inline def value: CFuncPtr3[CString, Ptr[linenoiseCompletions], Ptr[Byte], Unit] = v

  /** [bindgen] header: linenoise.h
    */
  type linenoiseFreeHintsCallback = CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]
  object linenoiseFreeHintsCallback:
    given _tag: Tag[linenoiseFreeHintsCallback] =
      Tag.materializeCFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]
    inline def apply(
        inline o: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]
    ): linenoiseFreeHintsCallback = o
    extension (v: linenoiseFreeHintsCallback) inline def value: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit] = v

  /** [bindgen] header: linenoise.h
    */
  type linenoiseHintsCallback =
    CFuncPtr4[CString, Ptr[CInt], Ptr[CInt], Ptr[Byte], CString]
  object linenoiseHintsCallback:
    given _tag: Tag[linenoiseHintsCallback] = Tag
      .materializeCFuncPtr4[CString, Ptr[CInt], Ptr[CInt], Ptr[Byte], CString]
    inline def apply(
        inline o: CFuncPtr4[CString, Ptr[CInt], Ptr[CInt], Ptr[Byte], CString]
    ): linenoiseHintsCallback = o
    extension (v: linenoiseHintsCallback) inline def value: CFuncPtr4[CString, Ptr[CInt], Ptr[CInt], Ptr[Byte], CString] = v

  type size_t = libc.stddef.size_t
  object size_t:
    val _tag: Tag[size_t] = summon[Tag[libc.stddef.size_t]]
    inline def apply(inline o: libc.stddef.size_t): size_t = o
    extension (v: size_t) inline def value: libc.stddef.size_t = v

object structs:
  import _root_.linenoise.aliases.*

  /** [bindgen] header: linenoise.h
    */
  opaque type linenoiseCompletions = CStruct2[size_t, Ptr[CString]]
  object linenoiseCompletions:
    given _tag: Tag[linenoiseCompletions] =
      Tag.materializeCStruct2Tag[size_t, Ptr[CString]]
    def apply()(using Zone): Ptr[linenoiseCompletions] =
      scala.scalanative.unsafe.alloc[linenoiseCompletions](1)
    def apply(len: size_t, cvec: Ptr[CString])(using
        Zone
    ): Ptr[linenoiseCompletions] =
      val ____ptr = apply()
      (!____ptr).len = len
      (!____ptr).cvec = cvec
      ____ptr
    extension (struct: linenoiseCompletions)
      def len: size_t = struct._1
      def len_=(value: size_t): Unit = !struct.at1 = value
      def cvec: Ptr[CString] = struct._2
      def cvec_=(value: Ptr[CString]): Unit = !struct.at2 = value

@extern
private[linenoise] object extern_functions:
  import _root_.linenoise.aliases.*
  import _root_.linenoise.structs.*

  /** [bindgen] header: linenoise.h
    */
  def linenoise(prompt: CString): CString = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseAddCompletion(
      comp: Ptr[linenoiseCompletions],
      str: CString
  ): Unit = extern

  /** Clear the screen.
    *
    * [bindgen] header: linenoise.h
    */
  def linenoiseClearScreen(): Unit = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseColumns(): CInt = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistory(len: Ptr[CInt]): Ptr[CString] = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistoryAdd(line: CString): CInt = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistoryFree(): Unit = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistoryGetMaxLen(): CInt = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistoryLoad(filename: CString): CInt = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistorySave(filename: CString): CInt = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseHistorySetMaxLen(len: CInt): CInt = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseSetCompletionCallback(
      comp: Ptr[linenoiseCompletionCallback],
      userdata: Ptr[Byte]
  ): Ptr[linenoiseCompletionCallback] = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseSetFreeHintsCallback(
      callback: Ptr[linenoiseFreeHintsCallback]
  ): Unit = extern

  /** [bindgen] header: linenoise.h
    */
  def linenoiseSetHintsCallback(
      callback: Ptr[linenoiseHintsCallback],
      userdata: Ptr[Byte]
  ): Unit = extern

  /** Enable or disable multiline mode (disabled by default)
    *
    * [bindgen] header: linenoise.h
    */
  def linenoiseSetMultiLine(enableml: CInt): Unit = extern

object functions:
  export extern_functions.*

object types:
  export _root_.linenoise.structs.*
  export _root_.linenoise.aliases.*

object all:
  export _root_.linenoise.aliases.linenoiseCompletionCallback
  export _root_.linenoise.aliases.linenoiseFreeHintsCallback
  export _root_.linenoise.aliases.linenoiseHintsCallback
  export _root_.linenoise.aliases.size_t
  export _root_.linenoise.structs.linenoiseCompletions
  export _root_.linenoise.functions.linenoise
  export _root_.linenoise.functions.linenoiseAddCompletion
  export _root_.linenoise.functions.linenoiseClearScreen
  export _root_.linenoise.functions.linenoiseColumns
  export _root_.linenoise.functions.linenoiseHistory
  export _root_.linenoise.functions.linenoiseHistoryAdd
  export _root_.linenoise.functions.linenoiseHistoryFree
  export _root_.linenoise.functions.linenoiseHistoryGetMaxLen
  export _root_.linenoise.functions.linenoiseHistoryLoad
  export _root_.linenoise.functions.linenoiseHistorySave
  export _root_.linenoise.functions.linenoiseHistorySetMaxLen
  export _root_.linenoise.functions.linenoiseSetCompletionCallback
  export _root_.linenoise.functions.linenoiseSetFreeHintsCallback
  export _root_.linenoise.functions.linenoiseSetHintsCallback
  export _root_.linenoise.functions.linenoiseSetMultiLine
