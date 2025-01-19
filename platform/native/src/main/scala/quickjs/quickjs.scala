// sn-bindgen-x86_64-pc-linux --package quickjs --header quickjs.h --scala > ~/Downloads/quickjs.scala
package quickjs

import _root_.scala.scalanative.unsafe.*
import _root_.scala.scalanative.unsigned.*
import _root_.scala.scalanative.*

object predef:
  private[quickjs] trait CEnumU[T](using eq: T =:= UInt):
    given Tag[T] = Tag.UInt.asInstanceOf[Tag[T]]
    extension (inline t: T)
      inline def int: CInt = eq.apply(t).toInt
      inline def uint: CUnsignedInt = eq.apply(t)
      inline def value: CUnsignedInt = eq.apply(t)

object enumerations:
  import predef.*

  /** [bindgen] header: quickjs.h
    */
  opaque type JSCFunctionEnum = CUnsignedInt
  object JSCFunctionEnum extends CEnumU[JSCFunctionEnum]:
    given _tag: Tag[JSCFunctionEnum] = Tag.UInt
    inline def define(inline a: Long): JSCFunctionEnum = a.toUInt
    val JS_CFUNC_generic = define(0)
    val JS_CFUNC_generic_magic = define(1)
    val JS_CFUNC_constructor = define(2)
    val JS_CFUNC_constructor_magic = define(3)
    val JS_CFUNC_constructor_or_func = define(4)
    val JS_CFUNC_constructor_or_func_magic = define(5)
    val JS_CFUNC_f_f = define(6)
    val JS_CFUNC_f_f_f = define(7)
    val JS_CFUNC_getter = define(8)
    val JS_CFUNC_setter = define(9)
    val JS_CFUNC_getter_magic = define(10)
    val JS_CFUNC_setter_magic = define(11)
    val JS_CFUNC_iterator_next = define(12)
    inline def getName(inline value: JSCFunctionEnum): Option[String] =
      inline value match
        case JS_CFUNC_generic                   => Some("JS_CFUNC_generic")
        case JS_CFUNC_generic_magic             => Some("JS_CFUNC_generic_magic")
        case JS_CFUNC_constructor               => Some("JS_CFUNC_constructor")
        case JS_CFUNC_constructor_magic         => Some("JS_CFUNC_constructor_magic")
        case JS_CFUNC_constructor_or_func       => Some("JS_CFUNC_constructor_or_func")
        case JS_CFUNC_constructor_or_func_magic => Some("JS_CFUNC_constructor_or_func_magic")
        case JS_CFUNC_f_f                       => Some("JS_CFUNC_f_f")
        case JS_CFUNC_f_f_f                     => Some("JS_CFUNC_f_f_f")
        case JS_CFUNC_getter                    => Some("JS_CFUNC_getter")
        case JS_CFUNC_setter                    => Some("JS_CFUNC_setter")
        case JS_CFUNC_getter_magic              => Some("JS_CFUNC_getter_magic")
        case JS_CFUNC_setter_magic              => Some("JS_CFUNC_setter_magic")
        case JS_CFUNC_iterator_next             => Some("JS_CFUNC_iterator_next")
        case _                                  => _root_.scala.None
    extension (a: JSCFunctionEnum)
      inline def &(b: JSCFunctionEnum): JSCFunctionEnum = a & b
      inline def |(b: JSCFunctionEnum): JSCFunctionEnum = a | b
      inline def is(b: JSCFunctionEnum): Boolean = (a & b) == b

  /** [bindgen] header: quickjs.h
    */
  opaque type JSPromiseStateEnum = CUnsignedInt
  object JSPromiseStateEnum extends CEnumU[JSPromiseStateEnum]:
    given _tag: Tag[JSPromiseStateEnum] = Tag.UInt
    inline def define(inline a: Long): JSPromiseStateEnum = a.toUInt
    val JS_PROMISE_PENDING = define(0)
    val JS_PROMISE_FULFILLED = define(1)
    val JS_PROMISE_REJECTED = define(2)
    inline def getName(inline value: JSPromiseStateEnum): Option[String] =
      inline value match
        case JS_PROMISE_PENDING   => Some("JS_PROMISE_PENDING")
        case JS_PROMISE_FULFILLED => Some("JS_PROMISE_FULFILLED")
        case JS_PROMISE_REJECTED  => Some("JS_PROMISE_REJECTED")
        case _                    => _root_.scala.None
    extension (a: JSPromiseStateEnum)
      inline def &(b: JSPromiseStateEnum): JSPromiseStateEnum = a & b
      inline def |(b: JSPromiseStateEnum): JSPromiseStateEnum = a | b
      inline def is(b: JSPromiseStateEnum): Boolean = (a & b) == b

  /** [bindgen] header: quickjs.h
    */
  opaque type JSTypedArrayEnum = CUnsignedInt
  object JSTypedArrayEnum extends CEnumU[JSTypedArrayEnum]:
    given _tag: Tag[JSTypedArrayEnum] = Tag.UInt
    inline def define(inline a: Long): JSTypedArrayEnum = a.toUInt
    val JS_TYPED_ARRAY_UINT8C = define(0)
    val JS_TYPED_ARRAY_INT8 = define(1)
    val JS_TYPED_ARRAY_UINT8 = define(2)
    val JS_TYPED_ARRAY_INT16 = define(3)
    val JS_TYPED_ARRAY_UINT16 = define(4)
    val JS_TYPED_ARRAY_INT32 = define(5)
    val JS_TYPED_ARRAY_UINT32 = define(6)
    val JS_TYPED_ARRAY_BIG_INT64 = define(7)
    val JS_TYPED_ARRAY_BIG_UINT64 = define(8)
    val JS_TYPED_ARRAY_FLOAT32 = define(9)
    val JS_TYPED_ARRAY_FLOAT64 = define(10)
    inline def getName(inline value: JSTypedArrayEnum): Option[String] =
      inline value match
        case JS_TYPED_ARRAY_UINT8C     => Some("JS_TYPED_ARRAY_UINT8C")
        case JS_TYPED_ARRAY_INT8       => Some("JS_TYPED_ARRAY_INT8")
        case JS_TYPED_ARRAY_UINT8      => Some("JS_TYPED_ARRAY_UINT8")
        case JS_TYPED_ARRAY_INT16      => Some("JS_TYPED_ARRAY_INT16")
        case JS_TYPED_ARRAY_UINT16     => Some("JS_TYPED_ARRAY_UINT16")
        case JS_TYPED_ARRAY_INT32      => Some("JS_TYPED_ARRAY_INT32")
        case JS_TYPED_ARRAY_UINT32     => Some("JS_TYPED_ARRAY_UINT32")
        case JS_TYPED_ARRAY_BIG_INT64  => Some("JS_TYPED_ARRAY_BIG_INT64")
        case JS_TYPED_ARRAY_BIG_UINT64 => Some("JS_TYPED_ARRAY_BIG_UINT64")
        case JS_TYPED_ARRAY_FLOAT32    => Some("JS_TYPED_ARRAY_FLOAT32")
        case JS_TYPED_ARRAY_FLOAT64    => Some("JS_TYPED_ARRAY_FLOAT64")
        case _                         => _root_.scala.None
    extension (a: JSTypedArrayEnum)
      inline def &(b: JSTypedArrayEnum): JSTypedArrayEnum = a & b
      inline def |(b: JSTypedArrayEnum): JSTypedArrayEnum = a | b
      inline def is(b: JSTypedArrayEnum): Boolean = (a & b) == b

object aliases:
  import _root_.quickjs.structs.*
  type FILE = libc.stdio.FILE
  object FILE:
    val _tag: Tag[FILE] = summon[Tag[libc.stdio.FILE]]
    inline def apply(inline o: libc.stdio.FILE): FILE = o
    extension (v: FILE) inline def value: libc.stdio.FILE = v

  /** [bindgen] header: quickjs.h
    */
  type JSAtom = uint32_t
  object JSAtom:
    given _tag: Tag[JSAtom] = uint32_t._tag
    inline def apply(inline o: uint32_t): JSAtom = o
    extension (v: JSAtom) inline def value: uint32_t = v

  /** [bindgen] header: quickjs.h
    */
  type JSCFunction = CFuncPtr4[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], JSValue]
  object JSCFunction:
    given _tag: Tag[JSCFunction] = Tag.materializeCFuncPtr4[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], JSValue]
    inline def apply(inline o: CFuncPtr4[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], JSValue]): JSCFunction = o
    extension (v: JSCFunction) inline def value: CFuncPtr4[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], JSValue] = v

  /** [bindgen] header: quickjs.h
    */
  type JSCFunctionData = CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, Ptr[JSValue], JSValue]
  object JSCFunctionData:
    given _tag: Tag[JSCFunctionData] = Tag.materializeCFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, Ptr[JSValue], JSValue]
    inline def apply(inline o: CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, Ptr[JSValue], JSValue]): JSCFunctionData = o
    extension (v: JSCFunctionData) inline def value: CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, Ptr[JSValue], JSValue] = v

  /** [bindgen] header: quickjs.h
    */
  type JSCFunctionMagic = CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]
  object JSCFunctionMagic:
    given _tag: Tag[JSCFunctionMagic] = Tag.materializeCFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]
    inline def apply(inline o: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]): JSCFunctionMagic = o
    extension (v: JSCFunctionMagic) inline def value: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue] = v

  /** [bindgen] header: quickjs.h
    */
  type JSClassCall = CFuncPtr6[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[JSValue], CInt, JSValue]
  object JSClassCall:
    given _tag: Tag[JSClassCall] = Tag.materializeCFuncPtr6[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[JSValue], CInt, JSValue]
    inline def apply(inline o: CFuncPtr6[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[JSValue], CInt, JSValue]): JSClassCall = o
    extension (v: JSClassCall) inline def value: CFuncPtr6[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[JSValue], CInt, JSValue] = v

  /** [bindgen] header: quickjs.h
    */
  type JSClassFinalizer = CFuncPtr2[Ptr[JSRuntime], JSValue, Unit]
  object JSClassFinalizer:
    given _tag: Tag[JSClassFinalizer] = Tag.materializeCFuncPtr2[Ptr[JSRuntime], JSValue, Unit]
    inline def apply(inline o: CFuncPtr2[Ptr[JSRuntime], JSValue, Unit]): JSClassFinalizer = o
    extension (v: JSClassFinalizer) inline def value: CFuncPtr2[Ptr[JSRuntime], JSValue, Unit] = v

  /** [bindgen] header: quickjs.h
    */
  type JSClassGCMark = CFuncPtr3[Ptr[JSRuntime], JSValue, Ptr[JS_MarkFunc], Unit]
  object JSClassGCMark:
    given _tag: Tag[JSClassGCMark] = Tag.materializeCFuncPtr3[Ptr[JSRuntime], JSValue, Ptr[JS_MarkFunc], Unit]
    inline def apply(inline o: CFuncPtr3[Ptr[JSRuntime], JSValue, Ptr[JS_MarkFunc], Unit]): JSClassGCMark = o
    extension (v: JSClassGCMark) inline def value: CFuncPtr3[Ptr[JSRuntime], JSValue, Ptr[JS_MarkFunc], Unit] = v

  /** [bindgen] header: quickjs.h
    */
  type JSClassID = uint32_t
  object JSClassID:
    given _tag: Tag[JSClassID] = uint32_t._tag
    inline def apply(inline o: uint32_t): JSClassID = o
    extension (v: JSClassID) inline def value: uint32_t = v

  /** [bindgen] header: quickjs.h
    */
  type JSFreeArrayBufferDataFunc = CFuncPtr3[Ptr[JSRuntime], Ptr[Byte], Ptr[Byte], Unit]
  object JSFreeArrayBufferDataFunc:
    given _tag: Tag[JSFreeArrayBufferDataFunc] = Tag.materializeCFuncPtr3[Ptr[JSRuntime], Ptr[Byte], Ptr[Byte], Unit]
    inline def apply(inline o: CFuncPtr3[Ptr[JSRuntime], Ptr[Byte], Ptr[Byte], Unit]): JSFreeArrayBufferDataFunc = o
    extension (v: JSFreeArrayBufferDataFunc) inline def value: CFuncPtr3[Ptr[JSRuntime], Ptr[Byte], Ptr[Byte], Unit] = v

  /** [bindgen] header: quickjs.h
    */
  type JSHostPromiseRejectionTracker = CFuncPtr5[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[Byte], Unit]
  object JSHostPromiseRejectionTracker:
    given _tag: Tag[JSHostPromiseRejectionTracker] = Tag.materializeCFuncPtr5[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[Byte], Unit]
    inline def apply(inline o: CFuncPtr5[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[Byte], Unit]): JSHostPromiseRejectionTracker = o
    extension (v: JSHostPromiseRejectionTracker) inline def value: CFuncPtr5[Ptr[JSContext], JSValue, JSValue, CInt, Ptr[Byte], Unit] = v

  /** [bindgen] header: quickjs.h
    */
  type JSInterruptHandler = CFuncPtr2[Ptr[JSRuntime], Ptr[Byte], CInt]
  object JSInterruptHandler:
    given _tag: Tag[JSInterruptHandler] = Tag.materializeCFuncPtr2[Ptr[JSRuntime], Ptr[Byte], CInt]
    inline def apply(inline o: CFuncPtr2[Ptr[JSRuntime], Ptr[Byte], CInt]): JSInterruptHandler = o
    extension (v: JSInterruptHandler) inline def value: CFuncPtr2[Ptr[JSRuntime], Ptr[Byte], CInt] = v

  /** [bindgen] header: quickjs.h
    */
  type JSJobFunc = CFuncPtr3[Ptr[JSContext], CInt, Ptr[JSValue], JSValue]
  object JSJobFunc:
    given _tag: Tag[JSJobFunc] = Tag.materializeCFuncPtr3[Ptr[JSContext], CInt, Ptr[JSValue], JSValue]
    inline def apply(inline o: CFuncPtr3[Ptr[JSContext], CInt, Ptr[JSValue], JSValue]): JSJobFunc = o
    extension (v: JSJobFunc) inline def value: CFuncPtr3[Ptr[JSContext], CInt, Ptr[JSValue], JSValue] = v

  /** [bindgen] header: quickjs.h
    */
  type JSModuleInitFunc = CFuncPtr2[Ptr[JSContext], Ptr[JSModuleDef], CInt]
  object JSModuleInitFunc:
    given _tag: Tag[JSModuleInitFunc] = Tag.materializeCFuncPtr2[Ptr[JSContext], Ptr[JSModuleDef], CInt]
    inline def apply(inline o: CFuncPtr2[Ptr[JSContext], Ptr[JSModuleDef], CInt]): JSModuleInitFunc = o
    extension (v: JSModuleInitFunc) inline def value: CFuncPtr2[Ptr[JSContext], Ptr[JSModuleDef], CInt] = v

  /** [bindgen] header: quickjs.h
    */
  type JSModuleLoaderFunc = CFuncPtr3[Ptr[JSContext], CString, Ptr[Byte], Ptr[JSModuleDef]]
  object JSModuleLoaderFunc:
    given _tag: Tag[JSModuleLoaderFunc] = Tag.materializeCFuncPtr3[Ptr[JSContext], CString, Ptr[Byte], Ptr[JSModuleDef]]
    inline def apply(inline o: CFuncPtr3[Ptr[JSContext], CString, Ptr[Byte], Ptr[JSModuleDef]]): JSModuleLoaderFunc = o
    extension (v: JSModuleLoaderFunc) inline def value: CFuncPtr3[Ptr[JSContext], CString, Ptr[Byte], Ptr[JSModuleDef]] = v

  /** [bindgen] header: quickjs.h
    */
  type JSModuleNormalizeFunc = CFuncPtr4[Ptr[JSContext], CString, CString, Ptr[Byte], CString]
  object JSModuleNormalizeFunc:
    given _tag: Tag[JSModuleNormalizeFunc] = Tag.materializeCFuncPtr4[Ptr[JSContext], CString, CString, Ptr[Byte], CString]
    inline def apply(inline o: CFuncPtr4[Ptr[JSContext], CString, CString, Ptr[Byte], CString]): JSModuleNormalizeFunc = o
    extension (v: JSModuleNormalizeFunc) inline def value: CFuncPtr4[Ptr[JSContext], CString, CString, Ptr[Byte], CString] = v

  /** [bindgen] header: quickjs.h
    */
  type JS_MarkFunc = CFuncPtr2[Ptr[JSRuntime], Ptr[JSGCObjectHeader], Unit]
  object JS_MarkFunc:
    given _tag: Tag[JS_MarkFunc] = Tag.materializeCFuncPtr2[Ptr[JSRuntime], Ptr[JSGCObjectHeader], Unit]
    inline def apply(inline o: CFuncPtr2[Ptr[JSRuntime], Ptr[JSGCObjectHeader], Unit]): JS_MarkFunc = o
    extension (v: JS_MarkFunc) inline def value: CFuncPtr2[Ptr[JSRuntime], Ptr[JSGCObjectHeader], Unit] = v

  type int16_t = scala.Short
  object int16_t:
    val _tag: Tag[int16_t] = summon[Tag[scala.Short]]
    inline def apply(inline o: scala.Short): int16_t = o
    extension (v: int16_t) inline def value: scala.Short = v

  type int32_t = scala.scalanative.unsafe.CInt
  object int32_t:
    val _tag: Tag[int32_t] = summon[Tag[scala.scalanative.unsafe.CInt]]
    inline def apply(inline o: scala.scalanative.unsafe.CInt): int32_t = o
    extension (v: int32_t) inline def value: scala.scalanative.unsafe.CInt = v

  type int64_t = scala.Long
  object int64_t:
    val _tag: Tag[int64_t] = summon[Tag[scala.Long]]
    inline def apply(inline o: scala.Long): int64_t = o
    extension (v: int64_t) inline def value: scala.Long = v

  type size_t = libc.stddef.size_t
  object size_t:
    val _tag: Tag[size_t] = summon[Tag[libc.stddef.size_t]]
    inline def apply(inline o: libc.stddef.size_t): size_t = o
    extension (v: size_t) inline def value: libc.stddef.size_t = v

  type uint32_t = scala.scalanative.unsigned.UInt
  object uint32_t:
    val _tag: Tag[uint32_t] = summon[Tag[scala.scalanative.unsigned.UInt]]
    inline def apply(inline o: scala.scalanative.unsigned.UInt): uint32_t = o
    extension (v: uint32_t) inline def value: scala.scalanative.unsigned.UInt = v

  type uint64_t = scala.scalanative.unsigned.ULong
  object uint64_t:
    val _tag: Tag[uint64_t] = summon[Tag[scala.scalanative.unsigned.ULong]]
    inline def apply(inline o: scala.scalanative.unsigned.ULong): uint64_t = o
    extension (v: uint64_t) inline def value: scala.scalanative.unsigned.ULong = v

  type uint8_t = scala.scalanative.unsigned.UByte
  object uint8_t:
    val _tag: Tag[uint8_t] = summon[Tag[scala.scalanative.unsigned.UByte]]
    inline def apply(inline o: scala.scalanative.unsigned.UByte): uint8_t = o
    extension (v: uint8_t) inline def value: scala.scalanative.unsigned.UByte = v

object structs:
  import _root_.quickjs.aliases.*
  import _root_.quickjs.unions.*

  /** [bindgen] header: quickjs.h
    */
  opaque type JSCFunctionListEntry = CStruct5[CString, uint8_t, uint8_t, int16_t, JSCFunctionListEntry.Union0]
  object JSCFunctionListEntry:
    /** [bindgen] header: quickjs.h
      */
    opaque type Union0 = CArray[Byte, Nat.Digit2[Nat._1, Nat._6]]
    object Union0:
      /** [bindgen] header: quickjs.h
        */
      opaque type Struct0 = CStruct3[uint8_t, uint8_t, JSCFunctionType]
      object Struct0:
        given _tag: Tag[Struct0] = Tag.materializeCStruct3Tag[uint8_t, uint8_t, JSCFunctionType]
        def apply()(using Zone): Ptr[Struct0] = scala.scalanative.unsafe.alloc[Struct0](1)
        def apply(length: uint8_t, cproto: uint8_t, cfunc: JSCFunctionType)(using Zone): Ptr[Struct0] =
          val ____ptr = apply()
          (!____ptr).length = length
          (!____ptr).cproto = cproto
          (!____ptr).cfunc = cfunc
          ____ptr
        extension (struct: Struct0)
          def length: uint8_t = struct._1
          def length_=(value: uint8_t): Unit = !struct.at1 = value
          def cproto: uint8_t = struct._2
          def cproto_=(value: uint8_t): Unit = !struct.at2 = value
          def cfunc: JSCFunctionType = struct._3
          def cfunc_=(value: JSCFunctionType): Unit = !struct.at3 = value

      /** [bindgen] header: quickjs.h
        */
      opaque type Struct1 = CStruct2[JSCFunctionType, JSCFunctionType]
      object Struct1:
        given _tag: Tag[Struct1] = Tag.materializeCStruct2Tag[JSCFunctionType, JSCFunctionType]
        def apply()(using Zone): Ptr[Struct1] = scala.scalanative.unsafe.alloc[Struct1](1)
        def apply(get: JSCFunctionType, set: JSCFunctionType)(using Zone): Ptr[Struct1] =
          val ____ptr = apply()
          (!____ptr).get = get
          (!____ptr).set = set
          ____ptr
        extension (struct: Struct1)
          def get: JSCFunctionType = struct._1
          def get_=(value: JSCFunctionType): Unit = !struct.at1 = value
          def set: JSCFunctionType = struct._2
          def set_=(value: JSCFunctionType): Unit = !struct.at2 = value

      /** [bindgen] header: quickjs.h
        */
      opaque type Struct2 = CStruct2[CString, CInt]
      object Struct2:
        given _tag: Tag[Struct2] = Tag.materializeCStruct2Tag[CString, CInt]
        def apply()(using Zone): Ptr[Struct2] = scala.scalanative.unsafe.alloc[Struct2](1)
        def apply(name: CString, base: CInt)(using Zone): Ptr[Struct2] =
          val ____ptr = apply()
          (!____ptr).name = name
          (!____ptr).base = base
          ____ptr
        extension (struct: Struct2)
          def name: CString = struct._1
          def name_=(value: CString): Unit = !struct.at1 = value
          def base: CInt = struct._2
          def base_=(value: CInt): Unit = !struct.at2 = value

      /** [bindgen] header: quickjs.h
        */
      opaque type Struct3 = CStruct2[Ptr[JSCFunctionListEntry], CInt]
      object Struct3:
        given _tag: Tag[Struct3] = Tag.materializeCStruct2Tag[Ptr[JSCFunctionListEntry], CInt]
        def apply()(using Zone): Ptr[Struct3] = scala.scalanative.unsafe.alloc[Struct3](1)
        def apply(tab: Ptr[JSCFunctionListEntry], len: CInt)(using Zone): Ptr[Struct3] =
          val ____ptr = apply()
          (!____ptr).tab = tab
          (!____ptr).len = len
          ____ptr
        extension (struct: Struct3)
          def tab: Ptr[JSCFunctionListEntry] = struct._1
          def tab_=(value: Ptr[JSCFunctionListEntry]): Unit = !struct.at1 = value
          def len: CInt = struct._2
          def len_=(value: CInt): Unit = !struct.at2 = value
      given _tag: Tag[Union0] = Tag.CArray[CChar, Nat.Digit2[Nat._1, Nat._6]](Tag.Byte, Tag.Digit2[Nat._1, Nat._6](Tag.Nat1, Tag.Nat6))
      def apply()(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        ___ptr
      @scala.annotation.targetName("apply_func")
      def apply(func: JSCFunctionListEntry.Union0.Struct0)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct0]].update(0, func)
        ___ptr
      @scala.annotation.targetName("apply_getset")
      def apply(getset: JSCFunctionListEntry.Union0.Struct1)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct1]].update(0, getset)
        ___ptr
      @scala.annotation.targetName("apply_alias")
      def apply(alias: JSCFunctionListEntry.Union0.Struct2)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct2]].update(0, alias)
        ___ptr
      @scala.annotation.targetName("apply_prop_list")
      def apply(prop_list: JSCFunctionListEntry.Union0.Struct3)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct3]].update(0, prop_list)
        ___ptr
      @scala.annotation.targetName("apply_str")
      def apply(str: CString)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[CString]].update(0, str)
        ___ptr
      @scala.annotation.targetName("apply_i32")
      def apply(i32: int32_t)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[int32_t]].update(0, i32)
        ___ptr
      @scala.annotation.targetName("apply_i64")
      def apply(i64: int64_t)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[int64_t]].update(0, i64)
        ___ptr
      @scala.annotation.targetName("apply_f64")
      def apply(f64: Double)(using Zone): Ptr[Union0] =
        val ___ptr = alloc[Union0](1)
        val un = !___ptr
        un.at(0).asInstanceOf[Ptr[Double]].update(0, f64)
        ___ptr
      extension (struct: Union0)
        def func: JSCFunctionListEntry.Union0.Struct0 = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct0]]
        def func_=(value: JSCFunctionListEntry.Union0.Struct0): Unit = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct0]] = value
        def getset: JSCFunctionListEntry.Union0.Struct1 = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct1]]
        def getset_=(value: JSCFunctionListEntry.Union0.Struct1): Unit = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct1]] = value
        def alias: JSCFunctionListEntry.Union0.Struct2 = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct2]]
        def alias_=(value: JSCFunctionListEntry.Union0.Struct2): Unit = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct2]] = value
        def prop_list: JSCFunctionListEntry.Union0.Struct3 = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct3]]
        def prop_list_=(value: JSCFunctionListEntry.Union0.Struct3): Unit = !struct.at(0).asInstanceOf[Ptr[JSCFunctionListEntry.Union0.Struct3]] =
          value
        def str: CString = !struct.at(0).asInstanceOf[Ptr[CString]]
        def str_=(value: CString): Unit = !struct.at(0).asInstanceOf[Ptr[CString]] = value
        def i32: int32_t = !struct.at(0).asInstanceOf[Ptr[int32_t]]
        def i32_=(value: int32_t): Unit = !struct.at(0).asInstanceOf[Ptr[int32_t]] = value
        def i64: int64_t = !struct.at(0).asInstanceOf[Ptr[int64_t]]
        def i64_=(value: int64_t): Unit = !struct.at(0).asInstanceOf[Ptr[int64_t]] = value
        def f64: Double = !struct.at(0).asInstanceOf[Ptr[Double]]
        def f64_=(value: Double): Unit = !struct.at(0).asInstanceOf[Ptr[Double]] = value
    given _tag: Tag[JSCFunctionListEntry] = Tag.materializeCStruct5Tag[CString, uint8_t, uint8_t, int16_t, JSCFunctionListEntry.Union0]
    def apply()(using Zone): Ptr[JSCFunctionListEntry] = scala.scalanative.unsafe.alloc[JSCFunctionListEntry](1)
    def apply(name: CString, prop_flags: uint8_t, def_type: uint8_t, magic: int16_t, u: JSCFunctionListEntry.Union0)(using
        Zone
    ): Ptr[JSCFunctionListEntry] =
      val ____ptr = apply()
      (!____ptr).name = name
      (!____ptr).prop_flags = prop_flags
      (!____ptr).def_type = def_type
      (!____ptr).magic = magic
      (!____ptr).u = u
      ____ptr
    extension (struct: JSCFunctionListEntry)
      def name: CString = struct._1
      def name_=(value: CString): Unit = !struct.at1 = value
      def prop_flags: uint8_t = struct._2
      def prop_flags_=(value: uint8_t): Unit = !struct.at2 = value
      def def_type: uint8_t = struct._3
      def def_type_=(value: uint8_t): Unit = !struct.at3 = value
      def magic: int16_t = struct._4
      def magic_=(value: int16_t): Unit = !struct.at4 = value
      def u: JSCFunctionListEntry.Union0 = struct._5
      def u_=(value: JSCFunctionListEntry.Union0): Unit = !struct.at5 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSClass = CStruct0
  object JSClass:
    given _tag: Tag[JSClass] = Tag.materializeCStruct0Tag

  /** [bindgen] header: quickjs.h
    */
  opaque type JSClassDef = CStruct5[CString, Ptr[JSClassFinalizer], Ptr[JSClassGCMark], Ptr[JSClassCall], Ptr[JSClassExoticMethods]]
  object JSClassDef:
    given _tag: Tag[JSClassDef] =
      Tag.materializeCStruct5Tag[CString, Ptr[JSClassFinalizer], Ptr[JSClassGCMark], Ptr[JSClassCall], Ptr[JSClassExoticMethods]]
    def apply()(using Zone): Ptr[JSClassDef] = scala.scalanative.unsafe.alloc[JSClassDef](1)
    def apply(
        class_name: CString,
        finalizer: Ptr[JSClassFinalizer],
        gc_mark: Ptr[JSClassGCMark],
        call: Ptr[JSClassCall],
        exotic: Ptr[JSClassExoticMethods]
    )(using Zone): Ptr[JSClassDef] =
      val ____ptr = apply()
      (!____ptr).class_name = class_name
      (!____ptr).finalizer = finalizer
      (!____ptr).gc_mark = gc_mark
      (!____ptr).call = call
      (!____ptr).exotic = exotic
      ____ptr
    extension (struct: JSClassDef)
      def class_name: CString = struct._1
      def class_name_=(value: CString): Unit = !struct.at1 = value
      def finalizer: Ptr[JSClassFinalizer] = struct._2
      def finalizer_=(value: Ptr[JSClassFinalizer]): Unit = !struct.at2 = value
      def gc_mark: Ptr[JSClassGCMark] = struct._3
      def gc_mark_=(value: Ptr[JSClassGCMark]): Unit = !struct.at3 = value
      def call: Ptr[JSClassCall] = struct._4
      def call_=(value: Ptr[JSClassCall]): Unit = !struct.at4 = value
      def exotic: Ptr[JSClassExoticMethods] = struct._5
      def exotic_=(value: Ptr[JSClassExoticMethods]): Unit = !struct.at5 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSClassExoticMethods = CStruct7[
    CFuncPtr4[Ptr[JSContext], Ptr[JSPropertyDescriptor], JSValue, JSAtom, CInt],
    CFuncPtr4[Ptr[JSContext], Ptr[Ptr[JSPropertyEnum]], Ptr[uint32_t], JSValue, CInt],
    CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt],
    CFuncPtr7[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, JSValue, CInt, CInt],
    CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt],
    CFuncPtr4[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue],
    CFuncPtr6[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, CInt, CInt]
  ]
  object JSClassExoticMethods:
    given _tag: Tag[JSClassExoticMethods] = Tag.materializeCStruct7Tag[
      CFuncPtr4[Ptr[JSContext], Ptr[JSPropertyDescriptor], JSValue, JSAtom, CInt],
      CFuncPtr4[Ptr[JSContext], Ptr[Ptr[JSPropertyEnum]], Ptr[uint32_t], JSValue, CInt],
      CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt],
      CFuncPtr7[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, JSValue, CInt, CInt],
      CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt],
      CFuncPtr4[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue],
      CFuncPtr6[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, CInt, CInt]
    ]
    def apply()(using Zone): Ptr[JSClassExoticMethods] = scala.scalanative.unsafe.alloc[JSClassExoticMethods](1)
    def apply(
        get_own_property: CFuncPtr4[Ptr[JSContext], Ptr[JSPropertyDescriptor], JSValue, JSAtom, CInt],
        get_own_property_names: CFuncPtr4[Ptr[JSContext], Ptr[Ptr[JSPropertyEnum]], Ptr[uint32_t], JSValue, CInt],
        delete_property: CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt],
        define_own_property: CFuncPtr7[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, JSValue, CInt, CInt],
        has_property: CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt],
        get_property: CFuncPtr4[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue],
        set_property: CFuncPtr6[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, CInt, CInt]
    )(using Zone): Ptr[JSClassExoticMethods] =
      val ____ptr = apply()
      (!____ptr).get_own_property = get_own_property
      (!____ptr).get_own_property_names = get_own_property_names
      (!____ptr).delete_property = delete_property
      (!____ptr).define_own_property = define_own_property
      (!____ptr).has_property = has_property
      (!____ptr).get_property = get_property
      (!____ptr).set_property = set_property
      ____ptr
    extension (struct: JSClassExoticMethods)
      def get_own_property: CFuncPtr4[Ptr[JSContext], Ptr[JSPropertyDescriptor], JSValue, JSAtom, CInt] = struct._1
      def get_own_property_=(value: CFuncPtr4[Ptr[JSContext], Ptr[JSPropertyDescriptor], JSValue, JSAtom, CInt]): Unit = !struct.at1 = value
      def get_own_property_names: CFuncPtr4[Ptr[JSContext], Ptr[Ptr[JSPropertyEnum]], Ptr[uint32_t], JSValue, CInt] = struct._2
      def get_own_property_names_=(value: CFuncPtr4[Ptr[JSContext], Ptr[Ptr[JSPropertyEnum]], Ptr[uint32_t], JSValue, CInt]): Unit = !struct.at2 =
        value
      def delete_property: CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt] = struct._3
      def delete_property_=(value: CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt]): Unit = !struct.at3 = value
      def define_own_property: CFuncPtr7[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, JSValue, CInt, CInt] = struct._4
      def define_own_property_=(value: CFuncPtr7[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, JSValue, CInt, CInt]): Unit = !struct.at4 = value
      def has_property: CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt] = struct._5
      def has_property_=(value: CFuncPtr3[Ptr[JSContext], JSValue, JSAtom, CInt]): Unit = !struct.at5 = value
      def get_property: CFuncPtr4[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue] = struct._6
      def get_property_=(value: CFuncPtr4[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue]): Unit = !struct.at6 = value
      def set_property: CFuncPtr6[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, CInt, CInt] = struct._7
      def set_property_=(value: CFuncPtr6[Ptr[JSContext], JSValue, JSAtom, JSValue, JSValue, CInt, CInt]): Unit = !struct.at7 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSContext = CStruct0
  object JSContext:
    given _tag: Tag[JSContext] = Tag.materializeCStruct0Tag

  /** [bindgen] header: quickjs.h
    */
  opaque type JSGCObjectHeader = CStruct0
  object JSGCObjectHeader:
    given _tag: Tag[JSGCObjectHeader] = Tag.materializeCStruct0Tag

  /** [bindgen] header: quickjs.h
    */
  opaque type JSMallocFunctions = CStruct4[
    CFuncPtr2[Ptr[JSMallocState], size_t, Ptr[Byte]],
    CFuncPtr2[Ptr[JSMallocState], Ptr[Byte], Unit],
    CFuncPtr3[Ptr[JSMallocState], Ptr[Byte], size_t, Ptr[Byte]],
    CFuncPtr1[Ptr[Byte], size_t]
  ]
  object JSMallocFunctions:
    given _tag: Tag[JSMallocFunctions] = Tag.materializeCStruct4Tag[
      CFuncPtr2[Ptr[JSMallocState], size_t, Ptr[Byte]],
      CFuncPtr2[Ptr[JSMallocState], Ptr[Byte], Unit],
      CFuncPtr3[Ptr[JSMallocState], Ptr[Byte], size_t, Ptr[Byte]],
      CFuncPtr1[Ptr[Byte], size_t]
    ]
    def apply()(using Zone): Ptr[JSMallocFunctions] = scala.scalanative.unsafe.alloc[JSMallocFunctions](1)
    def apply(
        js_malloc: CFuncPtr2[Ptr[JSMallocState], size_t, Ptr[Byte]],
        js_free: CFuncPtr2[Ptr[JSMallocState], Ptr[Byte], Unit],
        js_realloc: CFuncPtr3[Ptr[JSMallocState], Ptr[Byte], size_t, Ptr[Byte]],
        js_malloc_usable_size: CFuncPtr1[Ptr[Byte], size_t]
    )(using Zone): Ptr[JSMallocFunctions] =
      val ____ptr = apply()
      (!____ptr).js_malloc = js_malloc
      (!____ptr).js_free = js_free
      (!____ptr).js_realloc = js_realloc
      (!____ptr).js_malloc_usable_size = js_malloc_usable_size
      ____ptr
    extension (struct: JSMallocFunctions)
      def js_malloc: CFuncPtr2[Ptr[JSMallocState], size_t, Ptr[Byte]] = struct._1
      def js_malloc_=(value: CFuncPtr2[Ptr[JSMallocState], size_t, Ptr[Byte]]): Unit = !struct.at1 = value
      def js_free: CFuncPtr2[Ptr[JSMallocState], Ptr[Byte], Unit] = struct._2
      def js_free_=(value: CFuncPtr2[Ptr[JSMallocState], Ptr[Byte], Unit]): Unit = !struct.at2 = value
      def js_realloc: CFuncPtr3[Ptr[JSMallocState], Ptr[Byte], size_t, Ptr[Byte]] = struct._3
      def js_realloc_=(value: CFuncPtr3[Ptr[JSMallocState], Ptr[Byte], size_t, Ptr[Byte]]): Unit = !struct.at3 = value
      def js_malloc_usable_size: CFuncPtr1[Ptr[Byte], size_t] = struct._4
      def js_malloc_usable_size_=(value: CFuncPtr1[Ptr[Byte], size_t]): Unit = !struct.at4 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSMallocState = CStruct4[size_t, size_t, size_t, Ptr[Byte]]
  object JSMallocState:
    given _tag: Tag[JSMallocState] = Tag.materializeCStruct4Tag[size_t, size_t, size_t, Ptr[Byte]]
    def apply()(using Zone): Ptr[JSMallocState] = scala.scalanative.unsafe.alloc[JSMallocState](1)
    def apply(malloc_count: size_t, malloc_size: size_t, malloc_limit: size_t, opaque: Ptr[Byte])(using Zone): Ptr[JSMallocState] =
      val ____ptr = apply()
      (!____ptr).malloc_count = malloc_count
      (!____ptr).malloc_size = malloc_size
      (!____ptr).malloc_limit = malloc_limit
      (!____ptr).opaque = opaque
      ____ptr
    extension (struct: JSMallocState)
      def malloc_count: size_t = struct._1
      def malloc_count_=(value: size_t): Unit = !struct.at1 = value
      def malloc_size: size_t = struct._2
      def malloc_size_=(value: size_t): Unit = !struct.at2 = value
      def malloc_limit: size_t = struct._3
      def malloc_limit_=(value: size_t): Unit = !struct.at3 = value
      def opaque: Ptr[Byte] = struct._4
      def opaque_=(value: Ptr[Byte]): Unit = !struct.at4 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSMemoryUsage = CArray[CChar, Nat.Digit3[Nat._2, Nat._0, Nat._8]]
  object JSMemoryUsage:
    given _tag: Tag[JSMemoryUsage] =
      Tag.CArray[CChar, Nat.Digit3[Nat._2, Nat._0, Nat._8]](Tag.Byte, Tag.Digit3[Nat._2, Nat._0, Nat._8](Tag.Nat2, Tag.Nat0, Tag.Nat8))
    def apply()(using Zone): Ptr[JSMemoryUsage] = scala.scalanative.unsafe.alloc[JSMemoryUsage](1)
    def apply(
        malloc_size: int64_t,
        malloc_limit: int64_t,
        memory_used_size: int64_t,
        malloc_count: int64_t,
        memory_used_count: int64_t,
        atom_count: int64_t,
        atom_size: int64_t,
        str_count: int64_t,
        str_size: int64_t,
        obj_count: int64_t,
        obj_size: int64_t,
        prop_count: int64_t,
        prop_size: int64_t,
        shape_count: int64_t,
        shape_size: int64_t,
        js_func_count: int64_t,
        js_func_size: int64_t,
        js_func_code_size: int64_t,
        js_func_pc2line_count: int64_t,
        js_func_pc2line_size: int64_t,
        c_func_count: int64_t,
        array_count: int64_t,
        fast_array_count: int64_t,
        fast_array_elements: int64_t,
        binary_object_count: int64_t,
        binary_object_size: int64_t
    )(using Zone): Ptr[JSMemoryUsage] =
      val ____ptr = apply()
      (!____ptr).malloc_size = malloc_size
      (!____ptr).malloc_limit = malloc_limit
      (!____ptr).memory_used_size = memory_used_size
      (!____ptr).malloc_count = malloc_count
      (!____ptr).memory_used_count = memory_used_count
      (!____ptr).atom_count = atom_count
      (!____ptr).atom_size = atom_size
      (!____ptr).str_count = str_count
      (!____ptr).str_size = str_size
      (!____ptr).obj_count = obj_count
      (!____ptr).obj_size = obj_size
      (!____ptr).prop_count = prop_count
      (!____ptr).prop_size = prop_size
      (!____ptr).shape_count = shape_count
      (!____ptr).shape_size = shape_size
      (!____ptr).js_func_count = js_func_count
      (!____ptr).js_func_size = js_func_size
      (!____ptr).js_func_code_size = js_func_code_size
      (!____ptr).js_func_pc2line_count = js_func_pc2line_count
      (!____ptr).js_func_pc2line_size = js_func_pc2line_size
      (!____ptr).c_func_count = c_func_count
      (!____ptr).array_count = array_count
      (!____ptr).fast_array_count = fast_array_count
      (!____ptr).fast_array_elements = fast_array_elements
      (!____ptr).binary_object_count = binary_object_count
      (!____ptr).binary_object_size = binary_object_size
      ____ptr
    extension (struct: JSMemoryUsage)
      def malloc_size: int64_t = !struct.at(offsets(0)).asInstanceOf[Ptr[int64_t]]
      def malloc_size_=(value: int64_t): Unit = !struct.at(offsets(0)).asInstanceOf[Ptr[int64_t]] = value
      def malloc_limit: int64_t = !struct.at(offsets(1)).asInstanceOf[Ptr[int64_t]]
      def malloc_limit_=(value: int64_t): Unit = !struct.at(offsets(1)).asInstanceOf[Ptr[int64_t]] = value
      def memory_used_size: int64_t = !struct.at(offsets(2)).asInstanceOf[Ptr[int64_t]]
      def memory_used_size_=(value: int64_t): Unit = !struct.at(offsets(2)).asInstanceOf[Ptr[int64_t]] = value
      def malloc_count: int64_t = !struct.at(offsets(3)).asInstanceOf[Ptr[int64_t]]
      def malloc_count_=(value: int64_t): Unit = !struct.at(offsets(3)).asInstanceOf[Ptr[int64_t]] = value
      def memory_used_count: int64_t = !struct.at(offsets(4)).asInstanceOf[Ptr[int64_t]]
      def memory_used_count_=(value: int64_t): Unit = !struct.at(offsets(4)).asInstanceOf[Ptr[int64_t]] = value
      def atom_count: int64_t = !struct.at(offsets(5)).asInstanceOf[Ptr[int64_t]]
      def atom_count_=(value: int64_t): Unit = !struct.at(offsets(5)).asInstanceOf[Ptr[int64_t]] = value
      def atom_size: int64_t = !struct.at(offsets(6)).asInstanceOf[Ptr[int64_t]]
      def atom_size_=(value: int64_t): Unit = !struct.at(offsets(6)).asInstanceOf[Ptr[int64_t]] = value
      def str_count: int64_t = !struct.at(offsets(7)).asInstanceOf[Ptr[int64_t]]
      def str_count_=(value: int64_t): Unit = !struct.at(offsets(7)).asInstanceOf[Ptr[int64_t]] = value
      def str_size: int64_t = !struct.at(offsets(8)).asInstanceOf[Ptr[int64_t]]
      def str_size_=(value: int64_t): Unit = !struct.at(offsets(8)).asInstanceOf[Ptr[int64_t]] = value
      def obj_count: int64_t = !struct.at(offsets(9)).asInstanceOf[Ptr[int64_t]]
      def obj_count_=(value: int64_t): Unit = !struct.at(offsets(9)).asInstanceOf[Ptr[int64_t]] = value
      def obj_size: int64_t = !struct.at(offsets(10)).asInstanceOf[Ptr[int64_t]]
      def obj_size_=(value: int64_t): Unit = !struct.at(offsets(10)).asInstanceOf[Ptr[int64_t]] = value
      def prop_count: int64_t = !struct.at(offsets(11)).asInstanceOf[Ptr[int64_t]]
      def prop_count_=(value: int64_t): Unit = !struct.at(offsets(11)).asInstanceOf[Ptr[int64_t]] = value
      def prop_size: int64_t = !struct.at(offsets(12)).asInstanceOf[Ptr[int64_t]]
      def prop_size_=(value: int64_t): Unit = !struct.at(offsets(12)).asInstanceOf[Ptr[int64_t]] = value
      def shape_count: int64_t = !struct.at(offsets(13)).asInstanceOf[Ptr[int64_t]]
      def shape_count_=(value: int64_t): Unit = !struct.at(offsets(13)).asInstanceOf[Ptr[int64_t]] = value
      def shape_size: int64_t = !struct.at(offsets(14)).asInstanceOf[Ptr[int64_t]]
      def shape_size_=(value: int64_t): Unit = !struct.at(offsets(14)).asInstanceOf[Ptr[int64_t]] = value
      def js_func_count: int64_t = !struct.at(offsets(15)).asInstanceOf[Ptr[int64_t]]
      def js_func_count_=(value: int64_t): Unit = !struct.at(offsets(15)).asInstanceOf[Ptr[int64_t]] = value
      def js_func_size: int64_t = !struct.at(offsets(16)).asInstanceOf[Ptr[int64_t]]
      def js_func_size_=(value: int64_t): Unit = !struct.at(offsets(16)).asInstanceOf[Ptr[int64_t]] = value
      def js_func_code_size: int64_t = !struct.at(offsets(17)).asInstanceOf[Ptr[int64_t]]
      def js_func_code_size_=(value: int64_t): Unit = !struct.at(offsets(17)).asInstanceOf[Ptr[int64_t]] = value
      def js_func_pc2line_count: int64_t = !struct.at(offsets(18)).asInstanceOf[Ptr[int64_t]]
      def js_func_pc2line_count_=(value: int64_t): Unit = !struct.at(offsets(18)).asInstanceOf[Ptr[int64_t]] = value
      def js_func_pc2line_size: int64_t = !struct.at(offsets(19)).asInstanceOf[Ptr[int64_t]]
      def js_func_pc2line_size_=(value: int64_t): Unit = !struct.at(offsets(19)).asInstanceOf[Ptr[int64_t]] = value
      def c_func_count: int64_t = !struct.at(offsets(20)).asInstanceOf[Ptr[int64_t]]
      def c_func_count_=(value: int64_t): Unit = !struct.at(offsets(20)).asInstanceOf[Ptr[int64_t]] = value
      def array_count: int64_t = !struct.at(offsets(21)).asInstanceOf[Ptr[int64_t]]
      def array_count_=(value: int64_t): Unit = !struct.at(offsets(21)).asInstanceOf[Ptr[int64_t]] = value
      def fast_array_count: int64_t = !struct.at(offsets(22)).asInstanceOf[Ptr[int64_t]]
      def fast_array_count_=(value: int64_t): Unit = !struct.at(offsets(22)).asInstanceOf[Ptr[int64_t]] = value
      def fast_array_elements: int64_t = !struct.at(offsets(23)).asInstanceOf[Ptr[int64_t]]
      def fast_array_elements_=(value: int64_t): Unit = !struct.at(offsets(23)).asInstanceOf[Ptr[int64_t]] = value
      def binary_object_count: int64_t = !struct.at(offsets(24)).asInstanceOf[Ptr[int64_t]]
      def binary_object_count_=(value: int64_t): Unit = !struct.at(offsets(24)).asInstanceOf[Ptr[int64_t]] = value
      def binary_object_size: int64_t = !struct.at(offsets(25)).asInstanceOf[Ptr[int64_t]]
      def binary_object_size_=(value: int64_t): Unit = !struct.at(offsets(25)).asInstanceOf[Ptr[int64_t]] = value
    val offsets: Array[Int] =
      val res = Array.ofDim[Int](26)
      def align(offset: Int, alignment: Int) = {
        val alignmentMask = alignment - 1
        val padding =
          if ((offset & alignmentMask) == 0) 0
          else alignment - (offset & alignmentMask)
        offset + padding
      }

      res(0) = align(0, alignmentof[int64_t].toInt)
      res(1) = align(res(0) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(2) = align(res(1) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(3) = align(res(2) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(4) = align(res(3) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(5) = align(res(4) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(6) = align(res(5) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(7) = align(res(6) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(8) = align(res(7) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(9) = align(res(8) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(10) = align(res(9) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(11) = align(res(10) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(12) = align(res(11) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(13) = align(res(12) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(14) = align(res(13) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(15) = align(res(14) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(16) = align(res(15) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(17) = align(res(16) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(18) = align(res(17) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(19) = align(res(18) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(20) = align(res(19) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(21) = align(res(20) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(22) = align(res(21) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(23) = align(res(22) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(24) = align(res(23) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res(25) = align(res(24) + sizeof[int64_t].toInt, alignmentof[int64_t].toInt)
      res
    end offsets

  /** [bindgen] header: quickjs.h
    */
  opaque type JSModuleDef = CStruct0
  object JSModuleDef:
    given _tag: Tag[JSModuleDef] = Tag.materializeCStruct0Tag

  /** [bindgen] header: quickjs.h
    */
  opaque type JSObject = CStruct0
  object JSObject:
    given _tag: Tag[JSObject] = Tag.materializeCStruct0Tag

  /** [bindgen] header: quickjs.h
    */
  opaque type JSPropertyDescriptor = CStruct4[CInt, JSValue, JSValue, JSValue]
  object JSPropertyDescriptor:
    given _tag: Tag[JSPropertyDescriptor] = Tag.materializeCStruct4Tag[CInt, JSValue, JSValue, JSValue]
    def apply()(using Zone): Ptr[JSPropertyDescriptor] = scala.scalanative.unsafe.alloc[JSPropertyDescriptor](1)
    def apply(flags: CInt, value: JSValue, getter: JSValue, setter: JSValue)(using Zone): Ptr[JSPropertyDescriptor] =
      val ____ptr = apply()
      (!____ptr).flags = flags
      (!____ptr).value = value
      (!____ptr).getter = getter
      (!____ptr).setter = setter
      ____ptr
    extension (struct: JSPropertyDescriptor)
      def flags: CInt = struct._1
      def flags_=(value: CInt): Unit = !struct.at1 = value
      def value: JSValue = struct._2
      def value_=(value: JSValue): Unit = !struct.at2 = value
      def getter: JSValue = struct._3
      def getter_=(value: JSValue): Unit = !struct.at3 = value
      def setter: JSValue = struct._4
      def setter_=(value: JSValue): Unit = !struct.at4 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSPropertyEnum = CStruct2[CInt, JSAtom]
  object JSPropertyEnum:
    given _tag: Tag[JSPropertyEnum] = Tag.materializeCStruct2Tag[CInt, JSAtom]
    def apply()(using Zone): Ptr[JSPropertyEnum] = scala.scalanative.unsafe.alloc[JSPropertyEnum](1)
    def apply(is_enumerable: CInt, atom: JSAtom)(using Zone): Ptr[JSPropertyEnum] =
      val ____ptr = apply()
      (!____ptr).is_enumerable = is_enumerable
      (!____ptr).atom = atom
      ____ptr
    extension (struct: JSPropertyEnum)
      def is_enumerable: CInt = struct._1
      def is_enumerable_=(value: CInt): Unit = !struct.at1 = value
      def atom: JSAtom = struct._2
      def atom_=(value: JSAtom): Unit = !struct.at2 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSRefCountHeader = CStruct1[CInt]
  object JSRefCountHeader:
    given _tag: Tag[JSRefCountHeader] = Tag.materializeCStruct1Tag[CInt]
    def apply()(using Zone): Ptr[JSRefCountHeader] = scala.scalanative.unsafe.alloc[JSRefCountHeader](1)
    def apply(ref_count: CInt)(using Zone): Ptr[JSRefCountHeader] =
      val ____ptr = apply()
      (!____ptr).ref_count = ref_count
      ____ptr
    extension (struct: JSRefCountHeader)
      def ref_count: CInt = struct._1
      def ref_count_=(value: CInt): Unit = !struct.at1 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSRuntime = CStruct0
  object JSRuntime:
    given _tag: Tag[JSRuntime] = Tag.materializeCStruct0Tag

  /** [bindgen] header: quickjs.h
    */
  opaque type JSSharedArrayBufferFunctions =
    CStruct4[CFuncPtr2[Ptr[Byte], size_t, Ptr[Byte]], CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit], CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit], Ptr[Byte]]
  object JSSharedArrayBufferFunctions:
    given _tag: Tag[JSSharedArrayBufferFunctions] = Tag.materializeCStruct4Tag[CFuncPtr2[Ptr[Byte], size_t, Ptr[Byte]], CFuncPtr2[Ptr[Byte], Ptr[
      Byte
    ], Unit], CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit], Ptr[Byte]]
    def apply()(using Zone): Ptr[JSSharedArrayBufferFunctions] = scala.scalanative.unsafe.alloc[JSSharedArrayBufferFunctions](1)
    def apply(
        sab_alloc: CFuncPtr2[Ptr[Byte], size_t, Ptr[Byte]],
        sab_free: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit],
        sab_dup: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit],
        sab_opaque: Ptr[Byte]
    )(using Zone): Ptr[JSSharedArrayBufferFunctions] =
      val ____ptr = apply()
      (!____ptr).sab_alloc = sab_alloc
      (!____ptr).sab_free = sab_free
      (!____ptr).sab_dup = sab_dup
      (!____ptr).sab_opaque = sab_opaque
      ____ptr
    extension (struct: JSSharedArrayBufferFunctions)
      def sab_alloc: CFuncPtr2[Ptr[Byte], size_t, Ptr[Byte]] = struct._1
      def sab_alloc_=(value: CFuncPtr2[Ptr[Byte], size_t, Ptr[Byte]]): Unit = !struct.at1 = value
      def sab_free: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit] = struct._2
      def sab_free_=(value: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]): Unit = !struct.at2 = value
      def sab_dup: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit] = struct._3
      def sab_dup_=(value: CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]): Unit = !struct.at3 = value
      def sab_opaque: Ptr[Byte] = struct._4
      def sab_opaque_=(value: Ptr[Byte]): Unit = !struct.at4 = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSValue = CStruct2[JSValueUnion, int64_t]
  object JSValue:
    given _tag: Tag[JSValue] = Tag.materializeCStruct2Tag[JSValueUnion, int64_t]
    def apply()(using Zone): Ptr[JSValue] = scala.scalanative.unsafe.alloc[JSValue](1)
    def apply(u: JSValueUnion, tag: int64_t)(using Zone): Ptr[JSValue] =
      val ____ptr = apply()
      (!____ptr).u = u
      (!____ptr).tag = tag
      ____ptr
    extension (struct: JSValue)
      def u: JSValueUnion = struct._1
      def u_=(value: JSValueUnion): Unit = !struct.at1 = value
      def tag: int64_t = struct._2
      def tag_=(value: int64_t): Unit = !struct.at2 = value

object unions:
  import _root_.quickjs.aliases.*
  import _root_.quickjs.structs.*

  /** [bindgen] header: quickjs.h
    */
  opaque type JSCFunctionType = CArray[Byte, Nat._8]
  object JSCFunctionType:
    given _tag: Tag[JSCFunctionType] = Tag.CArray[CChar, Nat._8](Tag.Byte, Tag.Nat8)
    def apply()(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      ___ptr
    @scala.annotation.targetName("apply_generic")
    def apply(generic: Ptr[JSCFunction])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]].update(0, generic)
      ___ptr
    @scala.annotation.targetName("apply_generic_magic")
    def apply(generic_magic: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]]].update(0, generic_magic)
      ___ptr
    @scala.annotation.targetName("apply_constructor")
    def apply(constructor: Ptr[JSCFunction])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]].update(0, constructor)
      ___ptr
    @scala.annotation.targetName("apply_constructor_magic")
    def apply(constructor_magic: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]]].update(0, constructor_magic)
      ___ptr
    @scala.annotation.targetName("apply_constructor_or_func")
    def apply(constructor_or_func: Ptr[JSCFunction])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]].update(0, constructor_or_func)
      ___ptr
    @scala.annotation.targetName("apply_f_f")
    def apply(f_f: CFuncPtr1[Double, Double])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr1[Double, Double]]].update(0, f_f)
      ___ptr
    @scala.annotation.targetName("apply_f_f_f")
    def apply(f_f_f: CFuncPtr2[Double, Double, Double])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr2[Double, Double, Double]]].update(0, f_f_f)
      ___ptr
    @scala.annotation.targetName("apply_getter")
    def apply(getter: CFuncPtr2[Ptr[JSContext], JSValue, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr2[Ptr[JSContext], JSValue, JSValue]]].update(0, getter)
      ___ptr
    @scala.annotation.targetName("apply_setter")
    def apply(setter: CFuncPtr3[Ptr[JSContext], JSValue, JSValue, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr3[Ptr[JSContext], JSValue, JSValue, JSValue]]].update(0, setter)
      ___ptr
    @scala.annotation.targetName("apply_getter_magic")
    def apply(getter_magic: CFuncPtr3[Ptr[JSContext], JSValue, CInt, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr3[Ptr[JSContext], JSValue, CInt, JSValue]]].update(0, getter_magic)
      ___ptr
    @scala.annotation.targetName("apply_setter_magic")
    def apply(setter_magic: CFuncPtr4[Ptr[JSContext], JSValue, JSValue, CInt, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr4[Ptr[JSContext], JSValue, JSValue, CInt, JSValue]]].update(0, setter_magic)
      ___ptr
    @scala.annotation.targetName("apply_iterator_next")
    def apply(iterator_next: CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], Ptr[CInt], CInt, JSValue])(using Zone): Ptr[JSCFunctionType] =
      val ___ptr = alloc[JSCFunctionType](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], Ptr[CInt], CInt, JSValue]]].update(0, iterator_next)
      ___ptr
    extension (struct: JSCFunctionType)
      def generic: Ptr[JSCFunction] = !struct.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]]
      def generic_=(value: Ptr[JSCFunction]): Unit = !struct.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]] = value
      def generic_magic: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue] =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]]]
      def generic_magic_=(value: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]]] = value
      def constructor: Ptr[JSCFunction] = !struct.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]]
      def constructor_=(value: Ptr[JSCFunction]): Unit = !struct.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]] = value
      def constructor_magic: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue] =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]]]
      def constructor_magic_=(value: CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr5[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], CInt, JSValue]]] = value
      def constructor_or_func: Ptr[JSCFunction] = !struct.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]]
      def constructor_or_func_=(value: Ptr[JSCFunction]): Unit = !struct.at(0).asInstanceOf[Ptr[Ptr[JSCFunction]]] = value
      def f_f: CFuncPtr1[Double, Double] = !struct.at(0).asInstanceOf[Ptr[CFuncPtr1[Double, Double]]]
      def f_f_=(value: CFuncPtr1[Double, Double]): Unit = !struct.at(0).asInstanceOf[Ptr[CFuncPtr1[Double, Double]]] = value
      def f_f_f: CFuncPtr2[Double, Double, Double] = !struct.at(0).asInstanceOf[Ptr[CFuncPtr2[Double, Double, Double]]]
      def f_f_f_=(value: CFuncPtr2[Double, Double, Double]): Unit = !struct.at(0).asInstanceOf[Ptr[CFuncPtr2[Double, Double, Double]]] = value
      def getter: CFuncPtr2[Ptr[JSContext], JSValue, JSValue] = !struct.at(0).asInstanceOf[Ptr[CFuncPtr2[Ptr[JSContext], JSValue, JSValue]]]
      def getter_=(value: CFuncPtr2[Ptr[JSContext], JSValue, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr2[Ptr[JSContext], JSValue, JSValue]]] = value
      def setter: CFuncPtr3[Ptr[JSContext], JSValue, JSValue, JSValue] =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr3[Ptr[JSContext], JSValue, JSValue, JSValue]]]
      def setter_=(value: CFuncPtr3[Ptr[JSContext], JSValue, JSValue, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr3[Ptr[JSContext], JSValue, JSValue, JSValue]]] = value
      def getter_magic: CFuncPtr3[Ptr[JSContext], JSValue, CInt, JSValue] =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr3[Ptr[JSContext], JSValue, CInt, JSValue]]]
      def getter_magic_=(value: CFuncPtr3[Ptr[JSContext], JSValue, CInt, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr3[Ptr[JSContext], JSValue, CInt, JSValue]]] = value
      def setter_magic: CFuncPtr4[Ptr[JSContext], JSValue, JSValue, CInt, JSValue] =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr4[Ptr[JSContext], JSValue, JSValue, CInt, JSValue]]]
      def setter_magic_=(value: CFuncPtr4[Ptr[JSContext], JSValue, JSValue, CInt, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr4[Ptr[JSContext], JSValue, JSValue, CInt, JSValue]]] = value
      def iterator_next: CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], Ptr[CInt], CInt, JSValue] =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], Ptr[CInt], CInt, JSValue]]]
      def iterator_next_=(value: CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], Ptr[CInt], CInt, JSValue]): Unit =
        !struct.at(0).asInstanceOf[Ptr[CFuncPtr6[Ptr[JSContext], JSValue, CInt, Ptr[JSValue], Ptr[CInt], CInt, JSValue]]] = value

  /** [bindgen] header: quickjs.h
    */
  opaque type JSValueUnion = CArray[Byte, Nat._8]
  object JSValueUnion:
    given _tag: Tag[JSValueUnion] = Tag.CArray[CChar, Nat._8](Tag.Byte, Tag.Nat8)
    def apply()(using Zone): Ptr[JSValueUnion] =
      val ___ptr = alloc[JSValueUnion](1)
      ___ptr
    @scala.annotation.targetName("apply_int32")
    def apply(int32: int32_t)(using Zone): Ptr[JSValueUnion] =
      val ___ptr = alloc[JSValueUnion](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[int32_t]].update(0, int32)
      ___ptr
    @scala.annotation.targetName("apply_float64")
    def apply(float64: Double)(using Zone): Ptr[JSValueUnion] =
      val ___ptr = alloc[JSValueUnion](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[Double]].update(0, float64)
      ___ptr
    @scala.annotation.targetName("apply_ptr")
    def apply(ptr: Ptr[Byte])(using Zone): Ptr[JSValueUnion] =
      val ___ptr = alloc[JSValueUnion](1)
      val un = !___ptr
      un.at(0).asInstanceOf[Ptr[Ptr[Byte]]].update(0, ptr)
      ___ptr
    extension (struct: JSValueUnion)
      def int32: int32_t = !struct.at(0).asInstanceOf[Ptr[int32_t]]
      def int32_=(value: int32_t): Unit = !struct.at(0).asInstanceOf[Ptr[int32_t]] = value
      def float64: Double = !struct.at(0).asInstanceOf[Ptr[Double]]
      def float64_=(value: Double): Unit = !struct.at(0).asInstanceOf[Ptr[Double]] = value
      def ptr: Ptr[Byte] = !struct.at(0).asInstanceOf[Ptr[Ptr[Byte]]]
      def ptr_=(value: Ptr[Byte]): Unit = !struct.at(0).asInstanceOf[Ptr[Ptr[Byte]]] = value

@extern
private[quickjs] object extern_functions:
  import _root_.quickjs.enumerations.*
  import _root_.quickjs.aliases.*
  import _root_.quickjs.structs.*

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicBaseObjects(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicBigDecimal(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicBigFloat(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicBigInt(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicDate(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicEval(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicJSON(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicMapSet(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicOperators(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicPromise(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicProxy(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicRegExp(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicRegExpCompiler(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicStringNormalize(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddIntrinsicTypedArrays(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddModuleExport(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], name_str: CString): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AddModuleExportList(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], tab: Ptr[JSCFunctionListEntry], len: CInt): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_AtomToCString(ctx: Ptr[JSContext], atom: JSAtom): CString = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_ComputeMemoryUsage(rt: Ptr[JSRuntime], s: Ptr[JSMemoryUsage]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_DetectModule(input: CString, input_len: size_t): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_DumpMemoryUsage(fp: Ptr[FILE], s: Ptr[JSMemoryUsage], rt: Ptr[JSRuntime]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_DupAtom(ctx: Ptr[JSContext], v: JSAtom): JSAtom = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_DupContext(ctx: Ptr[JSContext]): Ptr[JSContext] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_EnableBignumExt(ctx: Ptr[JSContext], enable: CInt): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_EnqueueJob(ctx: Ptr[JSContext], job_func: Ptr[JSJobFunc], argc: CInt, argv: Ptr[JSValue]): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_ExecutePendingJob(rt: Ptr[JSRuntime], pctx: Ptr[Ptr[JSContext]]): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeAtom(ctx: Ptr[JSContext], v: JSAtom): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeAtomRT(rt: Ptr[JSRuntime], v: JSAtom): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeCString(ctx: Ptr[JSContext], ptr: CString): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeContext(s: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeRuntime(rt: Ptr[JSRuntime]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_GetContextOpaque(ctx: Ptr[JSContext]): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_GetModuleName(ctx: Ptr[JSContext], m: Ptr[JSModuleDef]): JSAtom = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_GetRuntime(ctx: Ptr[JSContext]): Ptr[JSRuntime] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_GetRuntimeOpaque(rt: Ptr[JSRuntime]): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_GetScriptOrModuleName(ctx: Ptr[JSContext], n_stack_levels: CInt): JSAtom = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_HasException(ctx: Ptr[JSContext]): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_IsJobPending(rt: Ptr[JSRuntime]): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_IsRegisteredClass(rt: Ptr[JSRuntime], class_id: JSClassID): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewAtom(ctx: Ptr[JSContext], str: CString): JSAtom = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewAtomLen(ctx: Ptr[JSContext], str: CString, len: size_t): JSAtom = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewAtomUInt32(ctx: Ptr[JSContext], n: uint32_t): JSAtom = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCModule(ctx: Ptr[JSContext], name_str: CString, func: Ptr[JSModuleInitFunc]): Ptr[JSModuleDef] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewClass(rt: Ptr[JSRuntime], class_id: JSClassID, class_def: Ptr[JSClassDef]): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewClassID(pclass_id: Ptr[JSClassID]): JSClassID = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewContext(rt: Ptr[JSRuntime]): Ptr[JSContext] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewContextRaw(rt: Ptr[JSRuntime]): Ptr[JSContext] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewRuntime(): Ptr[JSRuntime] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_NewRuntime2(mf: Ptr[JSMallocFunctions], opaque: Ptr[Byte]): Ptr[JSRuntime] = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_ResetUncatchableError(ctx: Ptr[JSContext]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_RunGC(rt: Ptr[JSRuntime]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetCanBlock(rt: Ptr[JSRuntime], can_block: CInt): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetContextOpaque(ctx: Ptr[JSContext], opaque: Ptr[Byte]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetGCThreshold(rt: Ptr[JSRuntime], gc_threshold: size_t): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetHostPromiseRejectionTracker(rt: Ptr[JSRuntime], cb: Ptr[JSHostPromiseRejectionTracker], opaque: Ptr[Byte]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetInterruptHandler(rt: Ptr[JSRuntime], cb: Ptr[JSInterruptHandler], opaque: Ptr[Byte]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetMaxStackSize(rt: Ptr[JSRuntime], stack_size: size_t): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetMemoryLimit(rt: Ptr[JSRuntime], limit: size_t): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetModuleExportList(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], tab: Ptr[JSCFunctionListEntry], len: CInt): CInt = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetModuleLoaderFunc(
      rt: Ptr[JSRuntime],
      module_normalize: Ptr[JSModuleNormalizeFunc],
      module_loader: Ptr[JSModuleLoaderFunc],
      opaque: Ptr[Byte]
  ): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetRuntimeInfo(rt: Ptr[JSRuntime], info: CString): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetRuntimeOpaque(rt: Ptr[JSRuntime], opaque: Ptr[Byte]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_SetSharedArrayBufferFunctions(rt: Ptr[JSRuntime], sf: Ptr[JSSharedArrayBufferFunctions]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def JS_UpdateStackTop(rt: Ptr[JSRuntime]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_AtomToString(ctx: Ptr[JSContext], atom: JSAtom, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_AtomToValue(ctx: Ptr[JSContext], atom: JSAtom, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_Call(
      ctx: Ptr[JSContext],
      func_obj: Ptr[JSValue],
      this_obj: Ptr[JSValue],
      argc: CInt,
      argv: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_CallConstructor(
      ctx: Ptr[JSContext],
      func_obj: Ptr[JSValue],
      argc: CInt,
      argv: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_CallConstructor2(
      ctx: Ptr[JSContext],
      func_obj: Ptr[JSValue],
      new_target: Ptr[JSValue],
      argc: CInt,
      argv: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DefineProperty(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      prop: JSAtom,
      `val`: Ptr[JSValue],
      getter: Ptr[JSValue],
      setter: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DefinePropertyGetSet(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      prop: JSAtom,
      getter: Ptr[JSValue],
      setter: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DefinePropertyValue(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      prop: JSAtom,
      `val`: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DefinePropertyValueStr(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      prop: CString,
      `val`: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DefinePropertyValueUint32(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      idx: uint32_t,
      `val`: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DeleteProperty(ctx: Ptr[JSContext], obj: Ptr[JSValue], prop: JSAtom, flags: CInt): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DetachArrayBuffer(ctx: Ptr[JSContext], obj: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DupValue(ctx: Ptr[JSContext], v: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_DupValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_Eval(
      ctx: Ptr[JSContext],
      input: CString,
      input_len: size_t,
      filename: CString,
      eval_flags: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_EvalFunction(ctx: Ptr[JSContext], fun_obj: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_EvalThis(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      input: CString,
      input_len: size_t,
      filename: CString,
      eval_flags: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_FreeValue(ctx: Ptr[JSContext], v: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_FreeValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetArrayBuffer(ctx: Ptr[JSContext], psize: Ptr[size_t], obj: Ptr[JSValue]): Ptr[uint8_t] = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetClassID(v: Ptr[JSValue]): JSClassID = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetClassProto(ctx: Ptr[JSContext], class_id: JSClassID, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetException(ctx: Ptr[JSContext], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetGlobalObject(ctx: Ptr[JSContext], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetImportMeta(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetModuleNamespace(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetOpaque(obj: Ptr[JSValue], class_id: JSClassID): Ptr[Byte] = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetOpaque2(ctx: Ptr[JSContext], obj: Ptr[JSValue], class_id: JSClassID): Ptr[Byte] = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetOwnProperty(
      ctx: Ptr[JSContext],
      desc: Ptr[JSPropertyDescriptor],
      obj: Ptr[JSValue],
      prop: JSAtom
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetOwnPropertyNames(
      ctx: Ptr[JSContext],
      ptab: Ptr[Ptr[JSPropertyEnum]],
      plen: Ptr[uint32_t],
      obj: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom, __return: Ptr[JSValue]): Unit =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetPropertyInternal(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      prop: JSAtom,
      receiver: Ptr[JSValue],
      throw_ref_error: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetPropertyStr(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: CString, __return: Ptr[JSValue]): Unit =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetPropertyUint32(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      idx: uint32_t,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetPrototype(ctx: Ptr[JSContext], `val`: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_GetTypedArrayBuffer(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      pbyte_offset: Ptr[size_t],
      pbyte_length: Ptr[size_t],
      pbytes_per_element: Ptr[size_t],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_HasProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_Invoke(
      ctx: Ptr[JSContext],
      this_val: Ptr[JSValue],
      atom: JSAtom,
      argc: CInt,
      argv: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsArray(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsBigDecimal(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsBigFloat(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsBigInt(ctx: Ptr[JSContext], v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsBool(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsConstructor(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsError(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsException(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsExtensible(ctx: Ptr[JSContext], obj: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsFunction(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsInstanceOf(ctx: Ptr[JSContext], `val`: Ptr[JSValue], obj: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsLiveObject(rt: Ptr[JSRuntime], obj: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsNull(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsNumber(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsObject(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsString(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsSymbol(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsUndefined(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_IsUninitialized(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_JSONStringify(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      replacer: Ptr[JSValue],
      space0: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_LoadModule(ctx: Ptr[JSContext], basename: CString, filename: CString, __return: Ptr[JSValue]): Unit =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_MarkValue(rt: Ptr[JSRuntime], `val`: Ptr[JSValue], mark_func: Ptr[JS_MarkFunc]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewArray(ctx: Ptr[JSContext], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewArrayBuffer(
      ctx: Ptr[JSContext],
      buf: Ptr[uint8_t],
      len: size_t,
      free_func: Ptr[JSFreeArrayBufferDataFunc],
      opaque: Ptr[Byte],
      is_shared: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewArrayBufferCopy(ctx: Ptr[JSContext], buf: Ptr[uint8_t], len: size_t, __return: Ptr[JSValue]): Unit =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewAtomString(ctx: Ptr[JSContext], str: CString, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewBigInt64(ctx: Ptr[JSContext], v: int64_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewBigUint64(ctx: Ptr[JSContext], v: uint64_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewBool(ctx: Ptr[JSContext], `val`: CInt, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewCFunction(
      ctx: Ptr[JSContext],
      func: Ptr[JSCFunction],
      name: CString,
      length: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewCFunction2(
      ctx: Ptr[JSContext],
      func: Ptr[JSCFunction],
      name: CString,
      length: CInt,
      cproto: JSCFunctionEnum,
      magic: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewCFunctionData(
      ctx: Ptr[JSContext],
      func: Ptr[JSCFunctionData],
      length: CInt,
      magic: CInt,
      data_len: CInt,
      data: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewCFunctionMagic(
      ctx: Ptr[JSContext],
      func: Ptr[JSCFunctionMagic],
      name: CString,
      length: CInt,
      cproto: JSCFunctionEnum,
      magic: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewCatchOffset(ctx: Ptr[JSContext], `val`: int32_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewDate(ctx: Ptr[JSContext], epoch_ms: Double, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewError(ctx: Ptr[JSContext], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewFloat64(ctx: Ptr[JSContext], d: Double, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewInt32(ctx: Ptr[JSContext], `val`: int32_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewInt64(ctx: Ptr[JSContext], `val`: int64_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewObject(ctx: Ptr[JSContext], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewObjectClass(ctx: Ptr[JSContext], class_id: CInt, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewObjectProto(ctx: Ptr[JSContext], proto: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewObjectProtoClass(
      ctx: Ptr[JSContext],
      proto: Ptr[JSValue],
      class_id: JSClassID,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewPromiseCapability(ctx: Ptr[JSContext], resolving_funcs: Ptr[JSValue], __return: Ptr[JSValue]): Unit =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewString(ctx: Ptr[JSContext], str: CString, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewStringLen(ctx: Ptr[JSContext], str1: CString, len1: size_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewTypedArray(
      ctx: Ptr[JSContext],
      argc: CInt,
      argv: Ptr[JSValue],
      array_type: JSTypedArrayEnum,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_NewUint32(ctx: Ptr[JSContext], `val`: uint32_t, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ParseJSON(
      ctx: Ptr[JSContext],
      buf: CString,
      buf_len: size_t,
      filename: CString,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ParseJSON2(
      ctx: Ptr[JSContext],
      buf: CString,
      buf_len: size_t,
      filename: CString,
      flags: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_PreventExtensions(ctx: Ptr[JSContext], obj: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_PromiseResult(ctx: Ptr[JSContext], promise: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_PromiseState(ctx: Ptr[JSContext], promise: Ptr[JSValue]): JSPromiseStateEnum = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ReadObject(
      ctx: Ptr[JSContext],
      buf: Ptr[uint8_t],
      buf_len: size_t,
      flags: CInt,
      __return: Ptr[JSValue]
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ResolveModule(ctx: Ptr[JSContext], obj: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SameValue(ctx: Ptr[JSContext], op1: Ptr[JSValue], op2: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SameValueZero(ctx: Ptr[JSContext], op1: Ptr[JSValue], op2: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetClassProto(ctx: Ptr[JSContext], class_id: JSClassID, obj: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetConstructor(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], proto: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetConstructorBit(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], `val`: CInt): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetIsHTMLDDA(ctx: Ptr[JSContext], obj: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetModuleExport(
      ctx: Ptr[JSContext],
      m: Ptr[JSModuleDef],
      export_name: CString,
      `val`: Ptr[JSValue]
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetOpaque(obj: Ptr[JSValue], opaque: Ptr[Byte]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom, `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetPropertyFunctionList(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      tab: Ptr[JSCFunctionListEntry],
      len: CInt
  ): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetPropertyInt64(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: int64_t, `val`: Ptr[JSValue]): CInt =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetPropertyInternal(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      prop: JSAtom,
      `val`: Ptr[JSValue],
      this_obj: Ptr[JSValue],
      flags: CInt
  ): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetPropertyStr(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: CString, `val`: Ptr[JSValue]): CInt =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetPropertyUint32(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: uint32_t, `val`: Ptr[JSValue]): CInt =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetPrototype(ctx: Ptr[JSContext], obj: Ptr[JSValue], proto_val: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_SetUncatchableError(ctx: Ptr[JSContext], `val`: Ptr[JSValue], flag: CInt): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_StrictEq(ctx: Ptr[JSContext], op1: Ptr[JSValue], op2: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_Throw(ctx: Ptr[JSContext], obj: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ThrowOutOfMemory(ctx: Ptr[JSContext], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToBigInt64(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToBool(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToCString(ctx: Ptr[JSContext], val1: Ptr[JSValue]): CString = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToCStringLen(ctx: Ptr[JSContext], plen: Ptr[size_t], val1: Ptr[JSValue]): CString = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToCStringLen2(ctx: Ptr[JSContext], plen: Ptr[size_t], val1: Ptr[JSValue], cesu8: CInt): CString = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToFloat64(ctx: Ptr[JSContext], pres: Ptr[Double], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToIndex(ctx: Ptr[JSContext], plen: Ptr[uint64_t], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToInt32(ctx: Ptr[JSContext], pres: Ptr[int32_t], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToInt64(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToInt64Ext(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToPropertyKey(ctx: Ptr[JSContext], `val`: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToString(ctx: Ptr[JSContext], `val`: Ptr[JSValue], __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ToUint32(ctx: Ptr[JSContext], pres: Ptr[uint32_t], `val`: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_VALUE_IS_NAN(v: Ptr[JSValue]): CInt = extern

  private[quickjs] def __sn_wrap_quickjs_JS_ValueToAtom(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): JSAtom = extern

  private[quickjs] def __sn_wrap_quickjs_JS_WriteObject(ctx: Ptr[JSContext], psize: Ptr[size_t], obj: Ptr[JSValue], flags: CInt): Ptr[uint8_t] =
    extern

  private[quickjs] def __sn_wrap_quickjs_JS_WriteObject2(
      ctx: Ptr[JSContext],
      psize: Ptr[size_t],
      obj: Ptr[JSValue],
      flags: CInt,
      psab_tab: Ptr[Ptr[Ptr[uint8_t]]],
      psab_tab_len: Ptr[size_t]
  ): Ptr[uint8_t] = extern

  private[quickjs] def __sn_wrap_quickjs___JS_FreeValue(ctx: Ptr[JSContext], v: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs___JS_FreeValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs___JS_NewFloat64(ctx: Ptr[JSContext], d: Double, __return: Ptr[JSValue]): Unit = extern

  private[quickjs] def __sn_wrap_quickjs_js_string_codePointRange(
      ctx: Ptr[JSContext],
      this_val: Ptr[JSValue],
      argc: CInt,
      argv: Ptr[JSValue],
      __return: Ptr[JSValue]
  ): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def js_free(ctx: Ptr[JSContext], ptr: Ptr[Byte]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def js_free_rt(rt: Ptr[JSRuntime], ptr: Ptr[Byte]): Unit = extern

  /** [bindgen] header: quickjs.h
    */
  def js_malloc(ctx: Ptr[JSContext], size: size_t): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_malloc_rt(rt: Ptr[JSRuntime], size: size_t): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_malloc_usable_size(ctx: Ptr[JSContext], ptr: Ptr[Byte]): size_t = extern

  /** [bindgen] header: quickjs.h
    */
  def js_malloc_usable_size_rt(rt: Ptr[JSRuntime], ptr: Ptr[Byte]): size_t = extern

  /** [bindgen] header: quickjs.h
    */
  def js_mallocz(ctx: Ptr[JSContext], size: size_t): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_mallocz_rt(rt: Ptr[JSRuntime], size: size_t): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_realloc(ctx: Ptr[JSContext], ptr: Ptr[Byte], size: size_t): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_realloc2(ctx: Ptr[JSContext], ptr: Ptr[Byte], size: size_t, pslack: Ptr[size_t]): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_realloc_rt(rt: Ptr[JSRuntime], ptr: Ptr[Byte], size: size_t): Ptr[Byte] = extern

  /** [bindgen] header: quickjs.h
    */
  def js_strdup(ctx: Ptr[JSContext], str: CString): CString = extern

  /** [bindgen] header: quickjs.h
    */
  def js_strndup(ctx: Ptr[JSContext], s: CString, n: size_t): CString = extern

object functions:
  import _root_.quickjs.enumerations.*
  import _root_.quickjs.aliases.*
  import _root_.quickjs.structs.*
  export extern_functions.*

  /** [bindgen] header: quickjs.h
    */
  def JS_AtomToString(ctx: Ptr[JSContext], atom: JSAtom)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_AtomToString(ctx, atom, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_AtomToString(ctx: Ptr[JSContext], atom: JSAtom)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_AtomToString(ctx, atom, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_AtomToValue(ctx: Ptr[JSContext], atom: JSAtom)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_AtomToValue(ctx, atom, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_AtomToValue(ctx: Ptr[JSContext], atom: JSAtom)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_AtomToValue(ctx, atom, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_Call(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], this_obj: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_Call(ctx, func_obj, this_obj, argc, argv, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_Call(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], this_obj: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_Call(ctx, func_obj, this_obj, argc, argv, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_Call(ctx: Ptr[JSContext], func_obj: JSValue, this_obj: JSValue, argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](3)
    !(__ptr_0 + 0) = func_obj
    !(__ptr_0 + 1) = this_obj
    __sn_wrap_quickjs_JS_Call(ctx, (__ptr_0 + 0), (__ptr_0 + 1), argc, argv, (__ptr_0 + 2))
    !(__ptr_0 + 2)

  /** [bindgen] header: quickjs.h
    */
  def JS_CallConstructor(ctx: Ptr[JSContext], func_obj: JSValue, argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = func_obj
    __sn_wrap_quickjs_JS_CallConstructor(ctx, (__ptr_0 + 0), argc, argv, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_CallConstructor(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_CallConstructor(ctx, func_obj, argc, argv, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_CallConstructor(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_CallConstructor(ctx, func_obj, argc, argv, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_CallConstructor2(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], new_target: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_CallConstructor2(ctx, func_obj, new_target, argc, argv, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_CallConstructor2(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], new_target: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(
      __return: Ptr[JSValue]
  ): Unit =
    __sn_wrap_quickjs_JS_CallConstructor2(ctx, func_obj, new_target, argc, argv, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_CallConstructor2(ctx: Ptr[JSContext], func_obj: JSValue, new_target: JSValue, argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](3)
    !(__ptr_0 + 0) = func_obj
    !(__ptr_0 + 1) = new_target
    __sn_wrap_quickjs_JS_CallConstructor2(ctx, (__ptr_0 + 0), (__ptr_0 + 1), argc, argv, (__ptr_0 + 2))
    !(__ptr_0 + 2)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefineProperty(ctx: Ptr[JSContext], this_obj: JSValue, prop: JSAtom, `val`: JSValue, getter: JSValue, setter: JSValue, flags: CInt)(using
      Zone
  ): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](4)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    !(__ptr_0 + 2) = getter
    !(__ptr_0 + 3) = setter
    __sn_wrap_quickjs_JS_DefineProperty(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1), (__ptr_0 + 2), (__ptr_0 + 3), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefineProperty(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      prop: JSAtom,
      `val`: Ptr[JSValue],
      getter: Ptr[JSValue],
      setter: Ptr[JSValue],
      flags: CInt
  ): CInt =
    __sn_wrap_quickjs_JS_DefineProperty(ctx, this_obj, prop, `val`, getter, setter, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyGetSet(ctx: Ptr[JSContext], this_obj: JSValue, prop: JSAtom, getter: JSValue, setter: JSValue, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](3)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = getter
    !(__ptr_0 + 2) = setter
    __sn_wrap_quickjs_JS_DefinePropertyGetSet(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1), (__ptr_0 + 2), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyGetSet(
      ctx: Ptr[JSContext],
      this_obj: Ptr[JSValue],
      prop: JSAtom,
      getter: Ptr[JSValue],
      setter: Ptr[JSValue],
      flags: CInt
  ): CInt =
    __sn_wrap_quickjs_JS_DefinePropertyGetSet(ctx, this_obj, prop, getter, setter, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyValue(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom, `val`: Ptr[JSValue], flags: CInt): CInt =
    __sn_wrap_quickjs_JS_DefinePropertyValue(ctx, this_obj, prop, `val`, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyValue(ctx: Ptr[JSContext], this_obj: JSValue, prop: JSAtom, `val`: JSValue, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_DefinePropertyValue(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyValueStr(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: CString, `val`: Ptr[JSValue], flags: CInt): CInt =
    __sn_wrap_quickjs_JS_DefinePropertyValueStr(ctx, this_obj, prop, `val`, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyValueStr(ctx: Ptr[JSContext], this_obj: JSValue, prop: CString, `val`: JSValue, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_DefinePropertyValueStr(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyValueUint32(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: uint32_t, `val`: Ptr[JSValue], flags: CInt): CInt =
    __sn_wrap_quickjs_JS_DefinePropertyValueUint32(ctx, this_obj, idx, `val`, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DefinePropertyValueUint32(ctx: Ptr[JSContext], this_obj: JSValue, idx: uint32_t, `val`: JSValue, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_DefinePropertyValueUint32(ctx, (__ptr_0 + 0), idx, (__ptr_0 + 1), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DeleteProperty(ctx: Ptr[JSContext], obj: Ptr[JSValue], prop: JSAtom, flags: CInt): CInt =
    __sn_wrap_quickjs_JS_DeleteProperty(ctx, obj, prop, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DeleteProperty(ctx: Ptr[JSContext], obj: JSValue, prop: JSAtom, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_DeleteProperty(ctx, (__ptr_0 + 0), prop, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_DetachArrayBuffer(ctx: Ptr[JSContext], obj: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_DetachArrayBuffer(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_DetachArrayBuffer(ctx: Ptr[JSContext], obj: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_DetachArrayBuffer(ctx, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_DupValue(ctx: Ptr[JSContext], v: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_DupValue(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_DupValue(ctx: Ptr[JSContext], v: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_DupValue(ctx, v, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_DupValue(ctx: Ptr[JSContext], v: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_DupValue(ctx, v, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_DupValueRT(rt: Ptr[JSRuntime], v: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_DupValueRT(rt, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_DupValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_DupValueRT(rt, v, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_DupValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_DupValueRT(rt, v, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_Eval(ctx: Ptr[JSContext], input: CString, input_len: size_t, filename: CString, eval_flags: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_Eval(ctx, input, input_len, filename, eval_flags, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_Eval(ctx: Ptr[JSContext], input: CString, input_len: size_t, filename: CString, eval_flags: CInt)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_Eval(ctx, input, input_len, filename, eval_flags, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_EvalFunction(ctx: Ptr[JSContext], fun_obj: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = fun_obj
    __sn_wrap_quickjs_JS_EvalFunction(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_EvalFunction(ctx: Ptr[JSContext], fun_obj: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_EvalFunction(ctx, fun_obj, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_EvalFunction(ctx: Ptr[JSContext], fun_obj: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_EvalFunction(ctx, fun_obj, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_EvalThis(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], input: CString, input_len: size_t, filename: CString, eval_flags: CInt)(
      __return: Ptr[JSValue]
  ): Unit =
    __sn_wrap_quickjs_JS_EvalThis(ctx, this_obj, input, input_len, filename, eval_flags, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_EvalThis(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], input: CString, input_len: size_t, filename: CString, eval_flags: CInt)(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_EvalThis(ctx, this_obj, input, input_len, filename, eval_flags, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_EvalThis(ctx: Ptr[JSContext], this_obj: JSValue, input: CString, input_len: size_t, filename: CString, eval_flags: CInt)(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    __sn_wrap_quickjs_JS_EvalThis(ctx, (__ptr_0 + 0), input, input_len, filename, eval_flags, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeValue(ctx: Ptr[JSContext], v: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_FreeValue(ctx, v)

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeValue(ctx: Ptr[JSContext], v: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_FreeValue(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_FreeValueRT(rt, v)

  /** [bindgen] header: quickjs.h
    */
  def JS_FreeValueRT(rt: Ptr[JSRuntime], v: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_FreeValueRT(rt, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_GetArrayBuffer(ctx: Ptr[JSContext], psize: Ptr[size_t], obj: Ptr[JSValue]): Ptr[uint8_t] =
    __sn_wrap_quickjs_JS_GetArrayBuffer(ctx, psize, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetArrayBuffer(ctx: Ptr[JSContext], psize: Ptr[size_t], obj: JSValue)(using Zone): Ptr[uint8_t] =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_GetArrayBuffer(ctx, psize, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_GetClassID(v: JSValue)(using Zone): JSClassID =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_GetClassID((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_GetClassID(v: Ptr[JSValue]): JSClassID =
    __sn_wrap_quickjs_JS_GetClassID(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetClassProto(ctx: Ptr[JSContext], class_id: JSClassID)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetClassProto(ctx, class_id, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetClassProto(ctx: Ptr[JSContext], class_id: JSClassID)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetClassProto(ctx, class_id, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetException(ctx: Ptr[JSContext])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetException(ctx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetException(ctx: Ptr[JSContext])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetException(ctx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetGlobalObject(ctx: Ptr[JSContext])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetGlobalObject(ctx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetGlobalObject(ctx: Ptr[JSContext])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetGlobalObject(ctx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetImportMeta(ctx: Ptr[JSContext], m: Ptr[JSModuleDef])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetImportMeta(ctx, m, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetImportMeta(ctx: Ptr[JSContext], m: Ptr[JSModuleDef])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetImportMeta(ctx, m, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetModuleNamespace(ctx: Ptr[JSContext], m: Ptr[JSModuleDef])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetModuleNamespace(ctx, m, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetModuleNamespace(ctx: Ptr[JSContext], m: Ptr[JSModuleDef])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetModuleNamespace(ctx, m, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOpaque(obj: Ptr[JSValue], class_id: JSClassID): Ptr[Byte] =
    __sn_wrap_quickjs_JS_GetOpaque(obj, class_id)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOpaque(obj: JSValue, class_id: JSClassID)(using Zone): Ptr[Byte] =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_GetOpaque((__ptr_0 + 0), class_id)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOpaque2(ctx: Ptr[JSContext], obj: JSValue, class_id: JSClassID)(using Zone): Ptr[Byte] =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_GetOpaque2(ctx, (__ptr_0 + 0), class_id)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOpaque2(ctx: Ptr[JSContext], obj: Ptr[JSValue], class_id: JSClassID): Ptr[Byte] =
    __sn_wrap_quickjs_JS_GetOpaque2(ctx, obj, class_id)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOwnProperty(ctx: Ptr[JSContext], desc: Ptr[JSPropertyDescriptor], obj: JSValue, prop: JSAtom)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_GetOwnProperty(ctx, desc, (__ptr_0 + 0), prop)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOwnProperty(ctx: Ptr[JSContext], desc: Ptr[JSPropertyDescriptor], obj: Ptr[JSValue], prop: JSAtom): CInt =
    __sn_wrap_quickjs_JS_GetOwnProperty(ctx, desc, obj, prop)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOwnPropertyNames(ctx: Ptr[JSContext], ptab: Ptr[Ptr[JSPropertyEnum]], plen: Ptr[uint32_t], obj: Ptr[JSValue], flags: CInt): CInt =
    __sn_wrap_quickjs_JS_GetOwnPropertyNames(ctx, ptab, plen, obj, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetOwnPropertyNames(ctx: Ptr[JSContext], ptab: Ptr[Ptr[JSPropertyEnum]], plen: Ptr[uint32_t], obj: JSValue, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_GetOwnPropertyNames(ctx, ptab, plen, (__ptr_0 + 0), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetProperty(ctx: Ptr[JSContext], this_obj: JSValue, prop: JSAtom)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    __sn_wrap_quickjs_JS_GetProperty(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetProperty(ctx, this_obj, prop, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetProperty(ctx, this_obj, prop, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyInternal(ctx: Ptr[JSContext], obj: Ptr[JSValue], prop: JSAtom, receiver: Ptr[JSValue], throw_ref_error: CInt)(
      __return: Ptr[JSValue]
  ): Unit =
    __sn_wrap_quickjs_JS_GetPropertyInternal(ctx, obj, prop, receiver, throw_ref_error, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyInternal(ctx: Ptr[JSContext], obj: JSValue, prop: JSAtom, receiver: JSValue, throw_ref_error: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](3)
    !(__ptr_0 + 0) = obj
    !(__ptr_0 + 1) = receiver
    __sn_wrap_quickjs_JS_GetPropertyInternal(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1), throw_ref_error, (__ptr_0 + 2))
    !(__ptr_0 + 2)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyInternal(ctx: Ptr[JSContext], obj: Ptr[JSValue], prop: JSAtom, receiver: Ptr[JSValue], throw_ref_error: CInt)(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetPropertyInternal(ctx, obj, prop, receiver, throw_ref_error, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyStr(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: CString)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetPropertyStr(ctx, this_obj, prop, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyStr(ctx: Ptr[JSContext], this_obj: JSValue, prop: CString)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    __sn_wrap_quickjs_JS_GetPropertyStr(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyStr(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: CString)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetPropertyStr(ctx, this_obj, prop, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyUint32(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: uint32_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetPropertyUint32(ctx, this_obj, idx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyUint32(ctx: Ptr[JSContext], this_obj: JSValue, idx: uint32_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    __sn_wrap_quickjs_JS_GetPropertyUint32(ctx, (__ptr_0 + 0), idx, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPropertyUint32(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: uint32_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetPropertyUint32(ctx, this_obj, idx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPrototype(ctx: Ptr[JSContext], `val`: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetPrototype(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPrototype(ctx: Ptr[JSContext], `val`: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetPrototype(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetPrototype(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_GetPrototype(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetTypedArrayBuffer(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      pbyte_offset: Ptr[size_t],
      pbyte_length: Ptr[size_t],
      pbytes_per_element: Ptr[size_t]
  )(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_GetTypedArrayBuffer(ctx, obj, pbyte_offset, pbyte_length, pbytes_per_element, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetTypedArrayBuffer(
      ctx: Ptr[JSContext],
      obj: Ptr[JSValue],
      pbyte_offset: Ptr[size_t],
      pbyte_length: Ptr[size_t],
      pbytes_per_element: Ptr[size_t]
  )(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_GetTypedArrayBuffer(ctx, obj, pbyte_offset, pbyte_length, pbytes_per_element, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_GetTypedArrayBuffer(
      ctx: Ptr[JSContext],
      obj: JSValue,
      pbyte_offset: Ptr[size_t],
      pbyte_length: Ptr[size_t],
      pbytes_per_element: Ptr[size_t]
  )(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_GetTypedArrayBuffer(ctx, (__ptr_0 + 0), pbyte_offset, pbyte_length, pbytes_per_element, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_HasProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom): CInt =
    __sn_wrap_quickjs_JS_HasProperty(ctx, this_obj, prop)

  /** [bindgen] header: quickjs.h
    */
  def JS_HasProperty(ctx: Ptr[JSContext], this_obj: JSValue, prop: JSAtom)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = this_obj
    __sn_wrap_quickjs_JS_HasProperty(ctx, (__ptr_0 + 0), prop)

  /** [bindgen] header: quickjs.h
    */
  def JS_Invoke(ctx: Ptr[JSContext], this_val: Ptr[JSValue], atom: JSAtom, argc: CInt, argv: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_Invoke(ctx, this_val, atom, argc, argv, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_Invoke(ctx: Ptr[JSContext], this_val: Ptr[JSValue], atom: JSAtom, argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_Invoke(ctx, this_val, atom, argc, argv, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_Invoke(ctx: Ptr[JSContext], this_val: JSValue, atom: JSAtom, argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_val
    __sn_wrap_quickjs_JS_Invoke(ctx, (__ptr_0 + 0), atom, argc, argv, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsArray(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_IsArray(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsArray(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsArray(ctx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBigDecimal(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsBigDecimal((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBigDecimal(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsBigDecimal(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBigFloat(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsBigFloat((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBigFloat(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsBigFloat(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBigInt(ctx: Ptr[JSContext], v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsBigInt(ctx, v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBigInt(ctx: Ptr[JSContext], v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsBigInt(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBool(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsBool(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsBool(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsBool((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsConstructor(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_IsConstructor(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsConstructor(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsConstructor(ctx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsError(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_IsError(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsError(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsError(ctx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsException(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsException(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsException(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsException((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsExtensible(ctx: Ptr[JSContext], obj: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsExtensible(ctx, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsExtensible(ctx: Ptr[JSContext], obj: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_IsExtensible(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsFunction(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_IsFunction(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsFunction(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsFunction(ctx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsInstanceOf(ctx: Ptr[JSContext], `val`: Ptr[JSValue], obj: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsInstanceOf(ctx, `val`, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsInstanceOf(ctx: Ptr[JSContext], `val`: JSValue, obj: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = `val`
    !(__ptr_0 + 1) = obj
    __sn_wrap_quickjs_JS_IsInstanceOf(ctx, (__ptr_0 + 0), (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsLiveObject(rt: Ptr[JSRuntime], obj: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_IsLiveObject(rt, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsLiveObject(rt: Ptr[JSRuntime], obj: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsLiveObject(rt, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsNull(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsNull((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsNull(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsNull(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsNumber(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsNumber((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsNumber(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsNumber(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsObject(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsObject((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsObject(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsObject(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsString(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsString(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsString(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsString((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsSymbol(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsSymbol(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsSymbol(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsSymbol((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsUndefined(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsUndefined(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_IsUndefined(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsUndefined((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsUninitialized(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_IsUninitialized((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_IsUninitialized(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_IsUninitialized(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_JSONStringify(ctx: Ptr[JSContext], obj: JSValue, replacer: JSValue, space0: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](4)
    !(__ptr_0 + 0) = obj
    !(__ptr_0 + 1) = replacer
    !(__ptr_0 + 2) = space0
    __sn_wrap_quickjs_JS_JSONStringify(ctx, (__ptr_0 + 0), (__ptr_0 + 1), (__ptr_0 + 2), (__ptr_0 + 3))
    !(__ptr_0 + 3)

  /** [bindgen] header: quickjs.h
    */
  def JS_JSONStringify(ctx: Ptr[JSContext], obj: Ptr[JSValue], replacer: Ptr[JSValue], space0: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_JSONStringify(ctx, obj, replacer, space0, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_JSONStringify(ctx: Ptr[JSContext], obj: Ptr[JSValue], replacer: Ptr[JSValue], space0: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_JSONStringify(ctx, obj, replacer, space0, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_LoadModule(ctx: Ptr[JSContext], basename: CString, filename: CString)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_LoadModule(ctx, basename, filename, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_LoadModule(ctx: Ptr[JSContext], basename: CString, filename: CString)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_LoadModule(ctx, basename, filename, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_MarkValue(rt: Ptr[JSRuntime], `val`: JSValue, mark_func: Ptr[JS_MarkFunc])(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_MarkValue(rt, (__ptr_0 + 0), mark_func)

  /** [bindgen] header: quickjs.h
    */
  def JS_MarkValue(rt: Ptr[JSRuntime], `val`: Ptr[JSValue], mark_func: Ptr[JS_MarkFunc]): Unit =
    __sn_wrap_quickjs_JS_MarkValue(rt, `val`, mark_func)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewArray(ctx: Ptr[JSContext])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewArray(ctx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewArray(ctx: Ptr[JSContext])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewArray(ctx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewArrayBuffer(
      ctx: Ptr[JSContext],
      buf: Ptr[uint8_t],
      len: size_t,
      free_func: Ptr[JSFreeArrayBufferDataFunc],
      opaque: Ptr[Byte],
      is_shared: CInt
  )(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewArrayBuffer(ctx, buf, len, free_func, opaque, is_shared, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewArrayBuffer(
      ctx: Ptr[JSContext],
      buf: Ptr[uint8_t],
      len: size_t,
      free_func: Ptr[JSFreeArrayBufferDataFunc],
      opaque: Ptr[Byte],
      is_shared: CInt
  )(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewArrayBuffer(ctx, buf, len, free_func, opaque, is_shared, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewArrayBufferCopy(ctx: Ptr[JSContext], buf: Ptr[uint8_t], len: size_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewArrayBufferCopy(ctx, buf, len, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewArrayBufferCopy(ctx: Ptr[JSContext], buf: Ptr[uint8_t], len: size_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewArrayBufferCopy(ctx, buf, len, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewAtomString(ctx: Ptr[JSContext], str: CString)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewAtomString(ctx, str, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewAtomString(ctx: Ptr[JSContext], str: CString)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewAtomString(ctx, str, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewBigInt64(ctx: Ptr[JSContext], v: int64_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewBigInt64(ctx, v, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewBigInt64(ctx: Ptr[JSContext], v: int64_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewBigInt64(ctx, v, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewBigUint64(ctx: Ptr[JSContext], v: uint64_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewBigUint64(ctx, v, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewBigUint64(ctx: Ptr[JSContext], v: uint64_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewBigUint64(ctx, v, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewBool(ctx: Ptr[JSContext], `val`: CInt)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewBool(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewBool(ctx: Ptr[JSContext], `val`: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewBool(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunction(ctx: Ptr[JSContext], func: Ptr[JSCFunction], name: CString, length: CInt)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewCFunction(ctx, func, name, length, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunction(ctx: Ptr[JSContext], func: Ptr[JSCFunction], name: CString, length: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewCFunction(ctx, func, name, length, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunction2(ctx: Ptr[JSContext], func: Ptr[JSCFunction], name: CString, length: CInt, cproto: JSCFunctionEnum, magic: CInt)(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewCFunction2(ctx, func, name, length, cproto, magic, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunction2(ctx: Ptr[JSContext], func: Ptr[JSCFunction], name: CString, length: CInt, cproto: JSCFunctionEnum, magic: CInt)(
      __return: Ptr[JSValue]
  ): Unit =
    __sn_wrap_quickjs_JS_NewCFunction2(ctx, func, name, length, cproto, magic, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunctionData(ctx: Ptr[JSContext], func: Ptr[JSCFunctionData], length: CInt, magic: CInt, data_len: CInt, data: Ptr[JSValue])(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewCFunctionData(ctx, func, length, magic, data_len, data, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunctionData(ctx: Ptr[JSContext], func: Ptr[JSCFunctionData], length: CInt, magic: CInt, data_len: CInt, data: Ptr[JSValue])(
      __return: Ptr[JSValue]
  ): Unit =
    __sn_wrap_quickjs_JS_NewCFunctionData(ctx, func, length, magic, data_len, data, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunctionMagic(ctx: Ptr[JSContext], func: Ptr[JSCFunctionMagic], name: CString, length: CInt, cproto: JSCFunctionEnum, magic: CInt)(using
      Zone
  ): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewCFunctionMagic(ctx, func, name, length, cproto, magic, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCFunctionMagic(ctx: Ptr[JSContext], func: Ptr[JSCFunctionMagic], name: CString, length: CInt, cproto: JSCFunctionEnum, magic: CInt)(
      __return: Ptr[JSValue]
  ): Unit =
    __sn_wrap_quickjs_JS_NewCFunctionMagic(ctx, func, name, length, cproto, magic, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCatchOffset(ctx: Ptr[JSContext], `val`: int32_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewCatchOffset(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewCatchOffset(ctx: Ptr[JSContext], `val`: int32_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewCatchOffset(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewDate(ctx: Ptr[JSContext], epoch_ms: Double)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewDate(ctx, epoch_ms, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewDate(ctx: Ptr[JSContext], epoch_ms: Double)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewDate(ctx, epoch_ms, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewError(ctx: Ptr[JSContext])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewError(ctx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewError(ctx: Ptr[JSContext])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewError(ctx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewFloat64(ctx: Ptr[JSContext], d: Double)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewFloat64(ctx, d, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewFloat64(ctx: Ptr[JSContext], d: Double)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewFloat64(ctx, d, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewInt32(ctx: Ptr[JSContext], `val`: int32_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewInt32(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewInt32(ctx: Ptr[JSContext], `val`: int32_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewInt32(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewInt64(ctx: Ptr[JSContext], `val`: int64_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewInt64(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewInt64(ctx: Ptr[JSContext], `val`: int64_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewInt64(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObject(ctx: Ptr[JSContext])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewObject(ctx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObject(ctx: Ptr[JSContext])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewObject(ctx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectClass(ctx: Ptr[JSContext], class_id: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewObjectClass(ctx, class_id, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectClass(ctx: Ptr[JSContext], class_id: CInt)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewObjectClass(ctx, class_id, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectProto(ctx: Ptr[JSContext], proto: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewObjectProto(ctx, proto, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectProto(ctx: Ptr[JSContext], proto: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewObjectProto(ctx, proto, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectProto(ctx: Ptr[JSContext], proto: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = proto
    __sn_wrap_quickjs_JS_NewObjectProto(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectProtoClass(ctx: Ptr[JSContext], proto: Ptr[JSValue], class_id: JSClassID)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewObjectProtoClass(ctx, proto, class_id, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectProtoClass(ctx: Ptr[JSContext], proto: Ptr[JSValue], class_id: JSClassID)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewObjectProtoClass(ctx, proto, class_id, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewObjectProtoClass(ctx: Ptr[JSContext], proto: JSValue, class_id: JSClassID)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = proto
    __sn_wrap_quickjs_JS_NewObjectProtoClass(ctx, (__ptr_0 + 0), class_id, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewPromiseCapability(ctx: Ptr[JSContext], resolving_funcs: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewPromiseCapability(ctx, resolving_funcs, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewPromiseCapability(ctx: Ptr[JSContext], resolving_funcs: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewPromiseCapability(ctx, resolving_funcs, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewString(ctx: Ptr[JSContext], str: CString)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewString(ctx, str, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewString(ctx: Ptr[JSContext], str: CString)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewString(ctx, str, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewStringLen(ctx: Ptr[JSContext], str1: CString, len1: size_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewStringLen(ctx, str1, len1, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewStringLen(ctx: Ptr[JSContext], str1: CString, len1: size_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewStringLen(ctx, str1, len1, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewTypedArray(ctx: Ptr[JSContext], argc: CInt, argv: Ptr[JSValue], array_type: JSTypedArrayEnum)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewTypedArray(ctx, argc, argv, array_type, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewTypedArray(ctx: Ptr[JSContext], argc: CInt, argv: Ptr[JSValue], array_type: JSTypedArrayEnum)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewTypedArray(ctx, argc, argv, array_type, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewUint32(ctx: Ptr[JSContext], `val`: uint32_t)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_NewUint32(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_NewUint32(ctx: Ptr[JSContext], `val`: uint32_t)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_NewUint32(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ParseJSON(ctx: Ptr[JSContext], buf: CString, buf_len: size_t, filename: CString)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_ParseJSON(ctx, buf, buf_len, filename, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_ParseJSON(ctx: Ptr[JSContext], buf: CString, buf_len: size_t, filename: CString)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_ParseJSON(ctx, buf, buf_len, filename, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ParseJSON2(ctx: Ptr[JSContext], buf: CString, buf_len: size_t, filename: CString, flags: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_ParseJSON2(ctx, buf, buf_len, filename, flags, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ParseJSON2(ctx: Ptr[JSContext], buf: CString, buf_len: size_t, filename: CString, flags: CInt)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_ParseJSON2(ctx, buf, buf_len, filename, flags, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_PreventExtensions(ctx: Ptr[JSContext], obj: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_PreventExtensions(ctx, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_PreventExtensions(ctx: Ptr[JSContext], obj: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_PreventExtensions(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_PromiseResult(ctx: Ptr[JSContext], promise: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = promise
    __sn_wrap_quickjs_JS_PromiseResult(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_PromiseResult(ctx: Ptr[JSContext], promise: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_PromiseResult(ctx, promise, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_PromiseResult(ctx: Ptr[JSContext], promise: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_PromiseResult(ctx, promise, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_PromiseState(ctx: Ptr[JSContext], promise: JSValue)(using Zone): JSPromiseStateEnum =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = promise
    __sn_wrap_quickjs_JS_PromiseState(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_PromiseState(ctx: Ptr[JSContext], promise: Ptr[JSValue]): JSPromiseStateEnum =
    __sn_wrap_quickjs_JS_PromiseState(ctx, promise)

  /** [bindgen] header: quickjs.h
    */
  def JS_ReadObject(ctx: Ptr[JSContext], buf: Ptr[uint8_t], buf_len: size_t, flags: CInt)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_ReadObject(ctx, buf, buf_len, flags, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ReadObject(ctx: Ptr[JSContext], buf: Ptr[uint8_t], buf_len: size_t, flags: CInt)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_ReadObject(ctx, buf, buf_len, flags, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_ResolveModule(ctx: Ptr[JSContext], obj: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_ResolveModule(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ResolveModule(ctx: Ptr[JSContext], obj: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ResolveModule(ctx, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_SameValue(ctx: Ptr[JSContext], op1: Ptr[JSValue], op2: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SameValue(ctx, op1, op2)

  /** [bindgen] header: quickjs.h
    */
  def JS_SameValue(ctx: Ptr[JSContext], op1: JSValue, op2: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = op1
    !(__ptr_0 + 1) = op2
    __sn_wrap_quickjs_JS_SameValue(ctx, (__ptr_0 + 0), (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SameValueZero(ctx: Ptr[JSContext], op1: Ptr[JSValue], op2: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SameValueZero(ctx, op1, op2)

  /** [bindgen] header: quickjs.h
    */
  def JS_SameValueZero(ctx: Ptr[JSContext], op1: JSValue, op2: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = op1
    !(__ptr_0 + 1) = op2
    __sn_wrap_quickjs_JS_SameValueZero(ctx, (__ptr_0 + 0), (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetClassProto(ctx: Ptr[JSContext], class_id: JSClassID, obj: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_SetClassProto(ctx, class_id, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetClassProto(ctx: Ptr[JSContext], class_id: JSClassID, obj: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_SetClassProto(ctx, class_id, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetConstructor(ctx: Ptr[JSContext], func_obj: JSValue, proto: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = func_obj
    !(__ptr_0 + 1) = proto
    __sn_wrap_quickjs_JS_SetConstructor(ctx, (__ptr_0 + 0), (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetConstructor(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], proto: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_SetConstructor(ctx, func_obj, proto)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetConstructorBit(ctx: Ptr[JSContext], func_obj: JSValue, `val`: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = func_obj
    __sn_wrap_quickjs_JS_SetConstructorBit(ctx, (__ptr_0 + 0), `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetConstructorBit(ctx: Ptr[JSContext], func_obj: Ptr[JSValue], `val`: CInt): CInt =
    __sn_wrap_quickjs_JS_SetConstructorBit(ctx, func_obj, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetIsHTMLDDA(ctx: Ptr[JSContext], obj: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_SetIsHTMLDDA(ctx, obj)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetIsHTMLDDA(ctx: Ptr[JSContext], obj: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_SetIsHTMLDDA(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetModuleExport(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], export_name: CString, `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_SetModuleExport(ctx, m, export_name, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetModuleExport(ctx: Ptr[JSContext], m: Ptr[JSModuleDef], export_name: CString, `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SetModuleExport(ctx, m, export_name, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetOpaque(obj: Ptr[JSValue], opaque: Ptr[Byte]): Unit =
    __sn_wrap_quickjs_JS_SetOpaque(obj, opaque)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetOpaque(obj: JSValue, opaque: Ptr[Byte])(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_SetOpaque((__ptr_0 + 0), opaque)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetProperty(ctx: Ptr[JSContext], this_obj: JSValue, prop: JSAtom, `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_SetProperty(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetProperty(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: JSAtom, `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SetProperty(ctx, this_obj, prop, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyFunctionList(ctx: Ptr[JSContext], obj: Ptr[JSValue], tab: Ptr[JSCFunctionListEntry], len: CInt): Unit =
    __sn_wrap_quickjs_JS_SetPropertyFunctionList(ctx, obj, tab, len)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyFunctionList(ctx: Ptr[JSContext], obj: JSValue, tab: Ptr[JSCFunctionListEntry], len: CInt)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_SetPropertyFunctionList(ctx, (__ptr_0 + 0), tab, len)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyInt64(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: int64_t, `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SetPropertyInt64(ctx, this_obj, idx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyInt64(ctx: Ptr[JSContext], this_obj: JSValue, idx: int64_t, `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_SetPropertyInt64(ctx, (__ptr_0 + 0), idx, (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyInternal(ctx: Ptr[JSContext], obj: JSValue, prop: JSAtom, `val`: JSValue, this_obj: JSValue, flags: CInt)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](3)
    !(__ptr_0 + 0) = obj
    !(__ptr_0 + 1) = `val`
    !(__ptr_0 + 2) = this_obj
    __sn_wrap_quickjs_JS_SetPropertyInternal(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1), (__ptr_0 + 2), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyInternal(ctx: Ptr[JSContext], obj: Ptr[JSValue], prop: JSAtom, `val`: Ptr[JSValue], this_obj: Ptr[JSValue], flags: CInt): CInt =
    __sn_wrap_quickjs_JS_SetPropertyInternal(ctx, obj, prop, `val`, this_obj, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyStr(ctx: Ptr[JSContext], this_obj: JSValue, prop: CString, `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_SetPropertyStr(ctx, (__ptr_0 + 0), prop, (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyStr(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], prop: CString, `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SetPropertyStr(ctx, this_obj, prop, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyUint32(ctx: Ptr[JSContext], this_obj: Ptr[JSValue], idx: uint32_t, `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SetPropertyUint32(ctx, this_obj, idx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPropertyUint32(ctx: Ptr[JSContext], this_obj: JSValue, idx: uint32_t, `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_obj
    !(__ptr_0 + 1) = `val`
    __sn_wrap_quickjs_JS_SetPropertyUint32(ctx, (__ptr_0 + 0), idx, (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPrototype(ctx: Ptr[JSContext], obj: JSValue, proto_val: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = obj
    !(__ptr_0 + 1) = proto_val
    __sn_wrap_quickjs_JS_SetPrototype(ctx, (__ptr_0 + 0), (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_SetPrototype(ctx: Ptr[JSContext], obj: Ptr[JSValue], proto_val: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_SetPrototype(ctx, obj, proto_val)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetUncatchableError(ctx: Ptr[JSContext], `val`: JSValue, flag: CInt)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_SetUncatchableError(ctx, (__ptr_0 + 0), flag)

  /** [bindgen] header: quickjs.h
    */
  def JS_SetUncatchableError(ctx: Ptr[JSContext], `val`: Ptr[JSValue], flag: CInt): Unit =
    __sn_wrap_quickjs_JS_SetUncatchableError(ctx, `val`, flag)

  /** [bindgen] header: quickjs.h
    */
  def JS_StrictEq(ctx: Ptr[JSContext], op1: JSValue, op2: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = op1
    !(__ptr_0 + 1) = op2
    __sn_wrap_quickjs_JS_StrictEq(ctx, (__ptr_0 + 0), (__ptr_0 + 1))

  /** [bindgen] header: quickjs.h
    */
  def JS_StrictEq(ctx: Ptr[JSContext], op1: Ptr[JSValue], op2: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_StrictEq(ctx, op1, op2)

  /** [bindgen] header: quickjs.h
    */
  def JS_Throw(ctx: Ptr[JSContext], obj: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_Throw(ctx, obj, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_Throw(ctx: Ptr[JSContext], obj: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_Throw(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_Throw(ctx: Ptr[JSContext], obj: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_Throw(ctx, obj, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_ThrowOutOfMemory(ctx: Ptr[JSContext])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_ThrowOutOfMemory(ctx, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ThrowOutOfMemory(ctx: Ptr[JSContext])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_ThrowOutOfMemory(ctx, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToBigInt64(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToBigInt64(ctx, pres, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToBigInt64(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToBigInt64(ctx, pres, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToBool(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToBool(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToBool(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToBool(ctx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToCString(ctx: Ptr[JSContext], val1: Ptr[JSValue]): CString =
    __sn_wrap_quickjs_JS_ToCString(ctx, val1)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToCString(ctx: Ptr[JSContext], val1: JSValue)(using Zone): CString =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = val1
    __sn_wrap_quickjs_JS_ToCString(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToCStringLen(ctx: Ptr[JSContext], plen: Ptr[size_t], val1: Ptr[JSValue]): CString =
    __sn_wrap_quickjs_JS_ToCStringLen(ctx, plen, val1)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToCStringLen(ctx: Ptr[JSContext], plen: Ptr[size_t], val1: JSValue)(using Zone): CString =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = val1
    __sn_wrap_quickjs_JS_ToCStringLen(ctx, plen, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToCStringLen2(ctx: Ptr[JSContext], plen: Ptr[size_t], val1: JSValue, cesu8: CInt)(using Zone): CString =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = val1
    __sn_wrap_quickjs_JS_ToCStringLen2(ctx, plen, (__ptr_0 + 0), cesu8)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToCStringLen2(ctx: Ptr[JSContext], plen: Ptr[size_t], val1: Ptr[JSValue], cesu8: CInt): CString =
    __sn_wrap_quickjs_JS_ToCStringLen2(ctx, plen, val1, cesu8)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToFloat64(ctx: Ptr[JSContext], pres: Ptr[Double], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToFloat64(ctx, pres, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToFloat64(ctx: Ptr[JSContext], pres: Ptr[Double], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToFloat64(ctx, pres, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToIndex(ctx: Ptr[JSContext], plen: Ptr[uint64_t], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToIndex(ctx, plen, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToIndex(ctx: Ptr[JSContext], plen: Ptr[uint64_t], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToIndex(ctx, plen, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToInt32(ctx: Ptr[JSContext], pres: Ptr[int32_t], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToInt32(ctx, pres, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToInt32(ctx: Ptr[JSContext], pres: Ptr[int32_t], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToInt32(ctx, pres, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToInt64(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToInt64(ctx, pres, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToInt64(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToInt64(ctx, pres, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToInt64Ext(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToInt64Ext(ctx, pres, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToInt64Ext(ctx: Ptr[JSContext], pres: Ptr[int64_t], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToInt64Ext(ctx, pres, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ToPropertyKey(ctx: Ptr[JSContext], `val`: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_ToPropertyKey(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToPropertyKey(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToPropertyKey(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToPropertyKey(ctx: Ptr[JSContext], `val`: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_ToPropertyKey(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToString(ctx: Ptr[JSContext], `val`: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_JS_ToString(ctx, `val`, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToString(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToString(ctx, (__ptr_0 + 0), (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToString(ctx: Ptr[JSContext], `val`: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_JS_ToString(ctx, `val`, __return)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToUint32(ctx: Ptr[JSContext], pres: Ptr[uint32_t], `val`: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_ToUint32(ctx, pres, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_ToUint32(ctx: Ptr[JSContext], pres: Ptr[uint32_t], `val`: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ToUint32(ctx, pres, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_VALUE_IS_NAN(v: JSValue)(using Zone): CInt =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs_JS_VALUE_IS_NAN((__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_VALUE_IS_NAN(v: Ptr[JSValue]): CInt =
    __sn_wrap_quickjs_JS_VALUE_IS_NAN(v)

  /** [bindgen] header: quickjs.h
    */
  def JS_ValueToAtom(ctx: Ptr[JSContext], `val`: JSValue)(using Zone): JSAtom =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = `val`
    __sn_wrap_quickjs_JS_ValueToAtom(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def JS_ValueToAtom(ctx: Ptr[JSContext], `val`: Ptr[JSValue]): JSAtom =
    __sn_wrap_quickjs_JS_ValueToAtom(ctx, `val`)

  /** [bindgen] header: quickjs.h
    */
  def JS_WriteObject(ctx: Ptr[JSContext], psize: Ptr[size_t], obj: Ptr[JSValue], flags: CInt): Ptr[uint8_t] =
    __sn_wrap_quickjs_JS_WriteObject(ctx, psize, obj, flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_WriteObject(ctx: Ptr[JSContext], psize: Ptr[size_t], obj: JSValue, flags: CInt)(using Zone): Ptr[uint8_t] =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_WriteObject(ctx, psize, (__ptr_0 + 0), flags)

  /** [bindgen] header: quickjs.h
    */
  def JS_WriteObject2(
      ctx: Ptr[JSContext],
      psize: Ptr[size_t],
      obj: Ptr[JSValue],
      flags: CInt,
      psab_tab: Ptr[Ptr[Ptr[uint8_t]]],
      psab_tab_len: Ptr[size_t]
  ): Ptr[uint8_t] =
    __sn_wrap_quickjs_JS_WriteObject2(ctx, psize, obj, flags, psab_tab, psab_tab_len)

  /** [bindgen] header: quickjs.h
    */
  def JS_WriteObject2(
      ctx: Ptr[JSContext],
      psize: Ptr[size_t],
      obj: JSValue,
      flags: CInt,
      psab_tab: Ptr[Ptr[Ptr[uint8_t]]],
      psab_tab_len: Ptr[size_t]
  )(using Zone): Ptr[uint8_t] =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = obj
    __sn_wrap_quickjs_JS_WriteObject2(ctx, psize, (__ptr_0 + 0), flags, psab_tab, psab_tab_len)

  /** [bindgen] header: quickjs.h
    */
  def __JS_FreeValue(ctx: Ptr[JSContext], v: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs___JS_FreeValue(ctx, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def __JS_FreeValue(ctx: Ptr[JSContext], v: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs___JS_FreeValue(ctx, v)

  /** [bindgen] header: quickjs.h
    */
  def __JS_FreeValueRT(rt: Ptr[JSRuntime], v: JSValue)(using Zone): Unit =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    !(__ptr_0 + 0) = v
    __sn_wrap_quickjs___JS_FreeValueRT(rt, (__ptr_0 + 0))

  /** [bindgen] header: quickjs.h
    */
  def __JS_FreeValueRT(rt: Ptr[JSRuntime], v: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs___JS_FreeValueRT(rt, v)

  /** [bindgen] header: quickjs.h
    */
  def __JS_NewFloat64(ctx: Ptr[JSContext], d: Double)(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs___JS_NewFloat64(ctx, d, __return)

  /** [bindgen] header: quickjs.h
    */
  def __JS_NewFloat64(ctx: Ptr[JSContext], d: Double)(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs___JS_NewFloat64(ctx, d, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def js_string_codePointRange(ctx: Ptr[JSContext], this_val: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](1)
    __sn_wrap_quickjs_js_string_codePointRange(ctx, this_val, argc, argv, (__ptr_0 + 0))
    !(__ptr_0 + 0)

  /** [bindgen] header: quickjs.h
    */
  def js_string_codePointRange(ctx: Ptr[JSContext], this_val: JSValue, argc: CInt, argv: Ptr[JSValue])(using Zone): JSValue =
    val __ptr_0: Ptr[JSValue] = alloc[JSValue](2)
    !(__ptr_0 + 0) = this_val
    __sn_wrap_quickjs_js_string_codePointRange(ctx, (__ptr_0 + 0), argc, argv, (__ptr_0 + 1))
    !(__ptr_0 + 1)

  /** [bindgen] header: quickjs.h
    */
  def js_string_codePointRange(ctx: Ptr[JSContext], this_val: Ptr[JSValue], argc: CInt, argv: Ptr[JSValue])(__return: Ptr[JSValue]): Unit =
    __sn_wrap_quickjs_js_string_codePointRange(ctx, this_val, argc, argv, __return)

object constants:
  val JS_TAG_FIRST: CInt = -11
  val JS_TAG_BIG_DECIMAL: CInt = -11
  val JS_TAG_BIG_INT: CInt = -10
  val JS_TAG_BIG_FLOAT: CInt = -9
  val JS_TAG_SYMBOL: CInt = -8
  val JS_TAG_STRING: CInt = -7
  val JS_TAG_MODULE: CInt = -3
  val JS_TAG_FUNCTION_BYTECODE: CInt = -2
  val JS_TAG_OBJECT: CInt = -1
  val JS_TAG_INT: CInt = 0
  val JS_TAG_BOOL: CInt = 1
  val JS_TAG_NULL: CInt = 2
  val JS_TAG_UNDEFINED: CInt = 3
  val JS_TAG_UNINITIALIZED: CInt = 4
  val JS_TAG_CATCH_OFFSET: CInt = 5
  val JS_TAG_EXCEPTION: CInt = 6
  val JS_TAG_FLOAT64: CInt = 7

object types:
  export _root_.quickjs.structs.*
  export _root_.quickjs.aliases.*
  export _root_.quickjs.unions.*
  export _root_.quickjs.enumerations.*

object all:
  export _root_.quickjs.enumerations.JSCFunctionEnum
  export _root_.quickjs.enumerations.JSPromiseStateEnum
  export _root_.quickjs.enumerations.JSTypedArrayEnum
  export _root_.quickjs.aliases.FILE
  export _root_.quickjs.aliases.JSAtom
  export _root_.quickjs.aliases.JSCFunction
  export _root_.quickjs.aliases.JSCFunctionData
  export _root_.quickjs.aliases.JSCFunctionMagic
  export _root_.quickjs.aliases.JSClassCall
  export _root_.quickjs.aliases.JSClassFinalizer
  export _root_.quickjs.aliases.JSClassGCMark
  export _root_.quickjs.aliases.JSClassID
  export _root_.quickjs.aliases.JSFreeArrayBufferDataFunc
  export _root_.quickjs.aliases.JSHostPromiseRejectionTracker
  export _root_.quickjs.aliases.JSInterruptHandler
  export _root_.quickjs.aliases.JSJobFunc
  export _root_.quickjs.aliases.JSModuleInitFunc
  export _root_.quickjs.aliases.JSModuleLoaderFunc
  export _root_.quickjs.aliases.JSModuleNormalizeFunc
  export _root_.quickjs.aliases.JS_MarkFunc
  export _root_.quickjs.aliases.int16_t
  export _root_.quickjs.aliases.int32_t
  export _root_.quickjs.aliases.int64_t
  export _root_.quickjs.aliases.size_t
  export _root_.quickjs.aliases.uint32_t
  export _root_.quickjs.aliases.uint64_t
  export _root_.quickjs.aliases.uint8_t
  export _root_.quickjs.structs.JSCFunctionListEntry
  export _root_.quickjs.structs.JSClass
  export _root_.quickjs.structs.JSClassDef
  export _root_.quickjs.structs.JSClassExoticMethods
  export _root_.quickjs.structs.JSContext
  export _root_.quickjs.structs.JSGCObjectHeader
  export _root_.quickjs.structs.JSMallocFunctions
  export _root_.quickjs.structs.JSMallocState
  export _root_.quickjs.structs.JSMemoryUsage
  export _root_.quickjs.structs.JSModuleDef
  export _root_.quickjs.structs.JSObject
  export _root_.quickjs.structs.JSPropertyDescriptor
  export _root_.quickjs.structs.JSPropertyEnum
  export _root_.quickjs.structs.JSRefCountHeader
  export _root_.quickjs.structs.JSRuntime
  export _root_.quickjs.structs.JSSharedArrayBufferFunctions
  export _root_.quickjs.structs.JSValue
  export _root_.quickjs.unions.JSCFunctionType
  export _root_.quickjs.unions.JSValueUnion
  export _root_.quickjs.functions.JS_AddIntrinsicBaseObjects
  export _root_.quickjs.functions.JS_AddIntrinsicBigDecimal
  export _root_.quickjs.functions.JS_AddIntrinsicBigFloat
  export _root_.quickjs.functions.JS_AddIntrinsicBigInt
  export _root_.quickjs.functions.JS_AddIntrinsicDate
  export _root_.quickjs.functions.JS_AddIntrinsicEval
  export _root_.quickjs.functions.JS_AddIntrinsicJSON
  export _root_.quickjs.functions.JS_AddIntrinsicMapSet
  export _root_.quickjs.functions.JS_AddIntrinsicOperators
  export _root_.quickjs.functions.JS_AddIntrinsicPromise
  export _root_.quickjs.functions.JS_AddIntrinsicProxy
  export _root_.quickjs.functions.JS_AddIntrinsicRegExp
  export _root_.quickjs.functions.JS_AddIntrinsicRegExpCompiler
  export _root_.quickjs.functions.JS_AddIntrinsicStringNormalize
  export _root_.quickjs.functions.JS_AddIntrinsicTypedArrays
  export _root_.quickjs.functions.JS_AddModuleExport
  export _root_.quickjs.functions.JS_AddModuleExportList
  export _root_.quickjs.functions.JS_AtomToCString
  export _root_.quickjs.functions.JS_ComputeMemoryUsage
  export _root_.quickjs.functions.JS_DetectModule
  export _root_.quickjs.functions.JS_DumpMemoryUsage
  export _root_.quickjs.functions.JS_DupAtom
  export _root_.quickjs.functions.JS_DupContext
  export _root_.quickjs.functions.JS_EnableBignumExt
  export _root_.quickjs.functions.JS_EnqueueJob
  export _root_.quickjs.functions.JS_ExecutePendingJob
  export _root_.quickjs.functions.JS_FreeAtom
  export _root_.quickjs.functions.JS_FreeAtomRT
  export _root_.quickjs.functions.JS_FreeCString
  export _root_.quickjs.functions.JS_FreeContext
  export _root_.quickjs.functions.JS_FreeRuntime
  export _root_.quickjs.functions.JS_GetContextOpaque
  export _root_.quickjs.functions.JS_GetModuleName
  export _root_.quickjs.functions.JS_GetRuntime
  export _root_.quickjs.functions.JS_GetRuntimeOpaque
  export _root_.quickjs.functions.JS_GetScriptOrModuleName
  export _root_.quickjs.functions.JS_HasException
  export _root_.quickjs.functions.JS_IsJobPending
  export _root_.quickjs.functions.JS_IsRegisteredClass
  export _root_.quickjs.functions.JS_NewAtom
  export _root_.quickjs.functions.JS_NewAtomLen
  export _root_.quickjs.functions.JS_NewAtomUInt32
  export _root_.quickjs.functions.JS_NewCModule
  export _root_.quickjs.functions.JS_NewClass
  export _root_.quickjs.functions.JS_NewClassID
  export _root_.quickjs.functions.JS_NewContext
  export _root_.quickjs.functions.JS_NewContextRaw
  export _root_.quickjs.functions.JS_NewRuntime
  export _root_.quickjs.functions.JS_NewRuntime2
  export _root_.quickjs.functions.JS_ResetUncatchableError
  export _root_.quickjs.functions.JS_RunGC
  export _root_.quickjs.functions.JS_SetCanBlock
  export _root_.quickjs.functions.JS_SetContextOpaque
  export _root_.quickjs.functions.JS_SetGCThreshold
  export _root_.quickjs.functions.JS_SetHostPromiseRejectionTracker
  export _root_.quickjs.functions.JS_SetInterruptHandler
  export _root_.quickjs.functions.JS_SetMaxStackSize
  export _root_.quickjs.functions.JS_SetMemoryLimit
  export _root_.quickjs.functions.JS_SetModuleExportList
  export _root_.quickjs.functions.JS_SetModuleLoaderFunc
  export _root_.quickjs.functions.JS_SetRuntimeInfo
  export _root_.quickjs.functions.JS_SetRuntimeOpaque
  export _root_.quickjs.functions.JS_SetSharedArrayBufferFunctions
  export _root_.quickjs.functions.JS_UpdateStackTop
  export _root_.quickjs.functions.js_free
  export _root_.quickjs.functions.js_free_rt
  export _root_.quickjs.functions.js_malloc
  export _root_.quickjs.functions.js_malloc_rt
  export _root_.quickjs.functions.js_malloc_usable_size
  export _root_.quickjs.functions.js_malloc_usable_size_rt
  export _root_.quickjs.functions.js_mallocz
  export _root_.quickjs.functions.js_mallocz_rt
  export _root_.quickjs.functions.js_realloc
  export _root_.quickjs.functions.js_realloc2
  export _root_.quickjs.functions.js_realloc_rt
  export _root_.quickjs.functions.js_strdup
  export _root_.quickjs.functions.js_strndup
  export _root_.quickjs.functions.JS_AtomToString
  export _root_.quickjs.functions.JS_AtomToValue
  export _root_.quickjs.functions.JS_Call
  export _root_.quickjs.functions.JS_CallConstructor
  export _root_.quickjs.functions.JS_CallConstructor2
  export _root_.quickjs.functions.JS_DefineProperty
  export _root_.quickjs.functions.JS_DefinePropertyGetSet
  export _root_.quickjs.functions.JS_DefinePropertyValue
  export _root_.quickjs.functions.JS_DefinePropertyValueStr
  export _root_.quickjs.functions.JS_DefinePropertyValueUint32
  export _root_.quickjs.functions.JS_DeleteProperty
  export _root_.quickjs.functions.JS_DetachArrayBuffer
  export _root_.quickjs.functions.JS_DupValue
  export _root_.quickjs.functions.JS_DupValueRT
  export _root_.quickjs.functions.JS_Eval
  export _root_.quickjs.functions.JS_EvalFunction
  export _root_.quickjs.functions.JS_EvalThis
  export _root_.quickjs.functions.JS_FreeValue
  export _root_.quickjs.functions.JS_FreeValueRT
  export _root_.quickjs.functions.JS_GetArrayBuffer
  export _root_.quickjs.functions.JS_GetClassID
  export _root_.quickjs.functions.JS_GetClassProto
  export _root_.quickjs.functions.JS_GetException
  export _root_.quickjs.functions.JS_GetGlobalObject
  export _root_.quickjs.functions.JS_GetImportMeta
  export _root_.quickjs.functions.JS_GetModuleNamespace
  export _root_.quickjs.functions.JS_GetOpaque
  export _root_.quickjs.functions.JS_GetOpaque2
  export _root_.quickjs.functions.JS_GetOwnProperty
  export _root_.quickjs.functions.JS_GetOwnPropertyNames
  export _root_.quickjs.functions.JS_GetProperty
  export _root_.quickjs.functions.JS_GetPropertyInternal
  export _root_.quickjs.functions.JS_GetPropertyStr
  export _root_.quickjs.functions.JS_GetPropertyUint32
  export _root_.quickjs.functions.JS_GetPrototype
  export _root_.quickjs.functions.JS_GetTypedArrayBuffer
  export _root_.quickjs.functions.JS_HasProperty
  export _root_.quickjs.functions.JS_Invoke
  export _root_.quickjs.functions.JS_IsArray
  export _root_.quickjs.functions.JS_IsBigDecimal
  export _root_.quickjs.functions.JS_IsBigFloat
  export _root_.quickjs.functions.JS_IsBigInt
  export _root_.quickjs.functions.JS_IsBool
  export _root_.quickjs.functions.JS_IsConstructor
  export _root_.quickjs.functions.JS_IsError
  export _root_.quickjs.functions.JS_IsException
  export _root_.quickjs.functions.JS_IsExtensible
  export _root_.quickjs.functions.JS_IsFunction
  export _root_.quickjs.functions.JS_IsInstanceOf
  export _root_.quickjs.functions.JS_IsLiveObject
  export _root_.quickjs.functions.JS_IsNull
  export _root_.quickjs.functions.JS_IsNumber
  export _root_.quickjs.functions.JS_IsObject
  export _root_.quickjs.functions.JS_IsString
  export _root_.quickjs.functions.JS_IsSymbol
  export _root_.quickjs.functions.JS_IsUndefined
  export _root_.quickjs.functions.JS_IsUninitialized
  export _root_.quickjs.functions.JS_JSONStringify
  export _root_.quickjs.functions.JS_LoadModule
  export _root_.quickjs.functions.JS_MarkValue
  export _root_.quickjs.functions.JS_NewArray
  export _root_.quickjs.functions.JS_NewArrayBuffer
  export _root_.quickjs.functions.JS_NewArrayBufferCopy
  export _root_.quickjs.functions.JS_NewAtomString
  export _root_.quickjs.functions.JS_NewBigInt64
  export _root_.quickjs.functions.JS_NewBigUint64
  export _root_.quickjs.functions.JS_NewBool
  export _root_.quickjs.functions.JS_NewCFunction
  export _root_.quickjs.functions.JS_NewCFunction2
  export _root_.quickjs.functions.JS_NewCFunctionData
  export _root_.quickjs.functions.JS_NewCFunctionMagic
  export _root_.quickjs.functions.JS_NewCatchOffset
  export _root_.quickjs.functions.JS_NewDate
  export _root_.quickjs.functions.JS_NewError
  export _root_.quickjs.functions.JS_NewFloat64
  export _root_.quickjs.functions.JS_NewInt32
  export _root_.quickjs.functions.JS_NewInt64
  export _root_.quickjs.functions.JS_NewObject
  export _root_.quickjs.functions.JS_NewObjectClass
  export _root_.quickjs.functions.JS_NewObjectProto
  export _root_.quickjs.functions.JS_NewObjectProtoClass
  export _root_.quickjs.functions.JS_NewPromiseCapability
  export _root_.quickjs.functions.JS_NewString
  export _root_.quickjs.functions.JS_NewStringLen
  export _root_.quickjs.functions.JS_NewTypedArray
  export _root_.quickjs.functions.JS_NewUint32
  export _root_.quickjs.functions.JS_ParseJSON
  export _root_.quickjs.functions.JS_ParseJSON2
  export _root_.quickjs.functions.JS_PreventExtensions
  export _root_.quickjs.functions.JS_PromiseResult
  export _root_.quickjs.functions.JS_PromiseState
  export _root_.quickjs.functions.JS_ReadObject
  export _root_.quickjs.functions.JS_ResolveModule
  export _root_.quickjs.functions.JS_SameValue
  export _root_.quickjs.functions.JS_SameValueZero
  export _root_.quickjs.functions.JS_SetClassProto
  export _root_.quickjs.functions.JS_SetConstructor
  export _root_.quickjs.functions.JS_SetConstructorBit
  export _root_.quickjs.functions.JS_SetIsHTMLDDA
  export _root_.quickjs.functions.JS_SetModuleExport
  export _root_.quickjs.functions.JS_SetOpaque
  export _root_.quickjs.functions.JS_SetProperty
  export _root_.quickjs.functions.JS_SetPropertyFunctionList
  export _root_.quickjs.functions.JS_SetPropertyInt64
  export _root_.quickjs.functions.JS_SetPropertyInternal
  export _root_.quickjs.functions.JS_SetPropertyStr
  export _root_.quickjs.functions.JS_SetPropertyUint32
  export _root_.quickjs.functions.JS_SetPrototype
  export _root_.quickjs.functions.JS_SetUncatchableError
  export _root_.quickjs.functions.JS_StrictEq
  export _root_.quickjs.functions.JS_Throw
  export _root_.quickjs.functions.JS_ThrowOutOfMemory
  export _root_.quickjs.functions.JS_ToBigInt64
  export _root_.quickjs.functions.JS_ToBool
  export _root_.quickjs.functions.JS_ToCString
  export _root_.quickjs.functions.JS_ToCStringLen
  export _root_.quickjs.functions.JS_ToCStringLen2
  export _root_.quickjs.functions.JS_ToFloat64
  export _root_.quickjs.functions.JS_ToIndex
  export _root_.quickjs.functions.JS_ToInt32
  export _root_.quickjs.functions.JS_ToInt64
  export _root_.quickjs.functions.JS_ToInt64Ext
  export _root_.quickjs.functions.JS_ToPropertyKey
  export _root_.quickjs.functions.JS_ToString
  export _root_.quickjs.functions.JS_ToUint32
  export _root_.quickjs.functions.JS_VALUE_IS_NAN
  export _root_.quickjs.functions.JS_ValueToAtom
  export _root_.quickjs.functions.JS_WriteObject
  export _root_.quickjs.functions.JS_WriteObject2
  export _root_.quickjs.functions.__JS_FreeValue
  export _root_.quickjs.functions.__JS_FreeValueRT
  export _root_.quickjs.functions.__JS_NewFloat64
  export _root_.quickjs.functions.js_string_codePointRange
