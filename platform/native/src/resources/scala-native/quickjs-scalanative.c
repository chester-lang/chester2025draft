#include "quickjs/quickjs.h"
// sn-bindgen-x86_64-pc-linux --package quickjs --header quickjs.h --c > ~/Downloads/quickjs-scalanative.c
#include <string.h>

void __sn_wrap_quickjs_JS_AtomToString(JSContext * ctx, JSAtom atom, JSValue *____return) {
  JSValue ____ret = JS_AtomToString(ctx, atom);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_AtomToValue(JSContext * ctx, JSAtom atom, JSValue *____return) {
  JSValue ____ret = JS_AtomToValue(ctx, atom);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_Call(JSContext * ctx, JSValue *func_obj, JSValue *this_obj, int argc, JSValue * argv, JSValue *____return) {
  JSValue ____ret = JS_Call(ctx, *func_obj, *this_obj, argc, argv);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_CallConstructor(JSContext * ctx, JSValue *func_obj, int argc, JSValue * argv, JSValue *____return) {
  JSValue ____ret = JS_CallConstructor(ctx, *func_obj, argc, argv);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_CallConstructor2(JSContext * ctx, JSValue *func_obj, JSValue *new_target, int argc, JSValue * argv, JSValue *____return) {
  JSValue ____ret = JS_CallConstructor2(ctx, *func_obj, *new_target, argc, argv);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_DefineProperty(JSContext * ctx, JSValue *this_obj, JSAtom prop, JSValue *val, JSValue *getter, JSValue *setter, int flags) {
 return JS_DefineProperty(ctx, *this_obj, prop, *val, *getter, *setter, flags);
};


int __sn_wrap_quickjs_JS_DefinePropertyGetSet(JSContext * ctx, JSValue *this_obj, JSAtom prop, JSValue *getter, JSValue *setter, int flags) {
 return JS_DefinePropertyGetSet(ctx, *this_obj, prop, *getter, *setter, flags);
};


int __sn_wrap_quickjs_JS_DefinePropertyValue(JSContext * ctx, JSValue *this_obj, JSAtom prop, JSValue *val, int flags) {
 return JS_DefinePropertyValue(ctx, *this_obj, prop, *val, flags);
};


int __sn_wrap_quickjs_JS_DefinePropertyValueStr(JSContext * ctx, JSValue *this_obj, const char * prop, JSValue *val, int flags) {
 return JS_DefinePropertyValueStr(ctx, *this_obj, prop, *val, flags);
};


int __sn_wrap_quickjs_JS_DefinePropertyValueUint32(JSContext * ctx, JSValue *this_obj, uint32_t idx, JSValue *val, int flags) {
 return JS_DefinePropertyValueUint32(ctx, *this_obj, idx, *val, flags);
};


int __sn_wrap_quickjs_JS_DeleteProperty(JSContext * ctx, JSValue *obj, JSAtom prop, int flags) {
 return JS_DeleteProperty(ctx, *obj, prop, flags);
};


void __sn_wrap_quickjs_JS_DetachArrayBuffer(JSContext * ctx, JSValue *obj) {
 JS_DetachArrayBuffer(ctx, *obj);
};


void __sn_wrap_quickjs_JS_DupValue(JSContext * ctx, JSValue *v, JSValue *____return) {
  JSValue ____ret = JS_DupValue(ctx, *v);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_DupValueRT(JSRuntime * rt, JSValue *v, JSValue *____return) {
  JSValue ____ret = JS_DupValueRT(rt, *v);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_Eval(JSContext * ctx, const char * input, size_t input_len, const char * filename, int eval_flags, JSValue *____return) {
  JSValue ____ret = JS_Eval(ctx, input, input_len, filename, eval_flags);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_EvalFunction(JSContext * ctx, JSValue *fun_obj, JSValue *____return) {
  JSValue ____ret = JS_EvalFunction(ctx, *fun_obj);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_EvalThis(JSContext * ctx, JSValue *this_obj, const char * input, size_t input_len, const char * filename, int eval_flags, JSValue *____return) {
  JSValue ____ret = JS_EvalThis(ctx, *this_obj, input, input_len, filename, eval_flags);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_FreeValue(JSContext * ctx, JSValue *v) {
 JS_FreeValue(ctx, *v);
};


void __sn_wrap_quickjs_JS_FreeValueRT(JSRuntime * rt, JSValue *v) {
 JS_FreeValueRT(rt, *v);
};


uint8_t * __sn_wrap_quickjs_JS_GetArrayBuffer(JSContext * ctx, size_t * psize, JSValue *obj) {
 return JS_GetArrayBuffer(ctx, psize, *obj);
};


JSClassID __sn_wrap_quickjs_JS_GetClassID(JSValue *v) {
 return JS_GetClassID(*v);
};


void __sn_wrap_quickjs_JS_GetClassProto(JSContext * ctx, JSClassID class_id, JSValue *____return) {
  JSValue ____ret = JS_GetClassProto(ctx, class_id);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetException(JSContext * ctx, JSValue *____return) {
  JSValue ____ret = JS_GetException(ctx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetGlobalObject(JSContext * ctx, JSValue *____return) {
  JSValue ____ret = JS_GetGlobalObject(ctx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetImportMeta(JSContext * ctx, JSModuleDef * m, JSValue *____return) {
  JSValue ____ret = JS_GetImportMeta(ctx, m);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetModuleNamespace(JSContext * ctx, JSModuleDef * m, JSValue *____return) {
  JSValue ____ret = JS_GetModuleNamespace(ctx, m);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void * __sn_wrap_quickjs_JS_GetOpaque(JSValue *obj, JSClassID class_id) {
 return JS_GetOpaque(*obj, class_id);
};


void * __sn_wrap_quickjs_JS_GetOpaque2(JSContext * ctx, JSValue *obj, JSClassID class_id) {
 return JS_GetOpaque2(ctx, *obj, class_id);
};


int __sn_wrap_quickjs_JS_GetOwnProperty(JSContext * ctx, JSPropertyDescriptor * desc, JSValue *obj, JSAtom prop) {
 return JS_GetOwnProperty(ctx, desc, *obj, prop);
};


int __sn_wrap_quickjs_JS_GetOwnPropertyNames(JSContext * ctx, JSPropertyEnum ** ptab, uint32_t * plen, JSValue *obj, int flags) {
 return JS_GetOwnPropertyNames(ctx, ptab, plen, *obj, flags);
};


void __sn_wrap_quickjs_JS_GetProperty(JSContext * ctx, JSValue *this_obj, JSAtom prop, JSValue *____return) {
  JSValue ____ret = JS_GetProperty(ctx, *this_obj, prop);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetPropertyInternal(JSContext * ctx, JSValue *obj, JSAtom prop, JSValue *receiver, int throw_ref_error, JSValue *____return) {
  JSValue ____ret = JS_GetPropertyInternal(ctx, *obj, prop, *receiver, throw_ref_error);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetPropertyStr(JSContext * ctx, JSValue *this_obj, const char * prop, JSValue *____return) {
  JSValue ____ret = JS_GetPropertyStr(ctx, *this_obj, prop);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetPropertyUint32(JSContext * ctx, JSValue *this_obj, uint32_t idx, JSValue *____return) {
  JSValue ____ret = JS_GetPropertyUint32(ctx, *this_obj, idx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetPrototype(JSContext * ctx, JSValue *val, JSValue *____return) {
  JSValue ____ret = JS_GetPrototype(ctx, *val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_GetTypedArrayBuffer(JSContext * ctx, JSValue *obj, size_t * pbyte_offset, size_t * pbyte_length, size_t * pbytes_per_element, JSValue *____return) {
  JSValue ____ret = JS_GetTypedArrayBuffer(ctx, *obj, pbyte_offset, pbyte_length, pbytes_per_element);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_HasProperty(JSContext * ctx, JSValue *this_obj, JSAtom prop) {
 return JS_HasProperty(ctx, *this_obj, prop);
};


void __sn_wrap_quickjs_JS_Invoke(JSContext * ctx, JSValue *this_val, JSAtom atom, int argc, JSValue * argv, JSValue *____return) {
  JSValue ____ret = JS_Invoke(ctx, *this_val, atom, argc, argv);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_IsArray(JSContext * ctx, JSValue *val) {
 return JS_IsArray(ctx, *val);
};


int __sn_wrap_quickjs_JS_IsBigDecimal(JSValue *v) {
 return JS_IsBigDecimal(*v);
};


int __sn_wrap_quickjs_JS_IsBigFloat(JSValue *v) {
 return JS_IsBigFloat(*v);
};


int __sn_wrap_quickjs_JS_IsBigInt(JSContext * ctx, JSValue *v) {
 return JS_IsBigInt(ctx, *v);
};


int __sn_wrap_quickjs_JS_IsBool(JSValue *v) {
 return JS_IsBool(*v);
};


int __sn_wrap_quickjs_JS_IsConstructor(JSContext * ctx, JSValue *val) {
 return JS_IsConstructor(ctx, *val);
};


int __sn_wrap_quickjs_JS_IsError(JSContext * ctx, JSValue *val) {
 return JS_IsError(ctx, *val);
};


int __sn_wrap_quickjs_JS_IsException(JSValue *v) {
 return JS_IsException(*v);
};


int __sn_wrap_quickjs_JS_IsExtensible(JSContext * ctx, JSValue *obj) {
 return JS_IsExtensible(ctx, *obj);
};


int __sn_wrap_quickjs_JS_IsFunction(JSContext * ctx, JSValue *val) {
 return JS_IsFunction(ctx, *val);
};


int __sn_wrap_quickjs_JS_IsInstanceOf(JSContext * ctx, JSValue *val, JSValue *obj) {
 return JS_IsInstanceOf(ctx, *val, *obj);
};


int __sn_wrap_quickjs_JS_IsLiveObject(JSRuntime * rt, JSValue *obj) {
 return JS_IsLiveObject(rt, *obj);
};


int __sn_wrap_quickjs_JS_IsNull(JSValue *v) {
 return JS_IsNull(*v);
};


int __sn_wrap_quickjs_JS_IsNumber(JSValue *v) {
 return JS_IsNumber(*v);
};


int __sn_wrap_quickjs_JS_IsObject(JSValue *v) {
 return JS_IsObject(*v);
};


int __sn_wrap_quickjs_JS_IsString(JSValue *v) {
 return JS_IsString(*v);
};


int __sn_wrap_quickjs_JS_IsSymbol(JSValue *v) {
 return JS_IsSymbol(*v);
};


int __sn_wrap_quickjs_JS_IsUndefined(JSValue *v) {
 return JS_IsUndefined(*v);
};


int __sn_wrap_quickjs_JS_IsUninitialized(JSValue *v) {
 return JS_IsUninitialized(*v);
};


void __sn_wrap_quickjs_JS_JSONStringify(JSContext * ctx, JSValue *obj, JSValue *replacer, JSValue *space0, JSValue *____return) {
  JSValue ____ret = JS_JSONStringify(ctx, *obj, *replacer, *space0);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_LoadModule(JSContext * ctx, const char * basename, const char * filename, JSValue *____return) {
  JSValue ____ret = JS_LoadModule(ctx, basename, filename);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_MarkValue(JSRuntime * rt, JSValue *val, JS_MarkFunc * mark_func) {
 JS_MarkValue(rt, *val, mark_func);
};


void __sn_wrap_quickjs_JS_NewArray(JSContext * ctx, JSValue *____return) {
  JSValue ____ret = JS_NewArray(ctx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewArrayBuffer(JSContext * ctx, uint8_t * buf, size_t len, JSFreeArrayBufferDataFunc * free_func, void * opaque, int is_shared, JSValue *____return) {
  JSValue ____ret = JS_NewArrayBuffer(ctx, buf, len, free_func, opaque, is_shared);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewArrayBufferCopy(JSContext * ctx, const uint8_t * buf, size_t len, JSValue *____return) {
  JSValue ____ret = JS_NewArrayBufferCopy(ctx, buf, len);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewAtomString(JSContext * ctx, const char * str, JSValue *____return) {
  JSValue ____ret = JS_NewAtomString(ctx, str);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewBigInt64(JSContext * ctx, int64_t v, JSValue *____return) {
  JSValue ____ret = JS_NewBigInt64(ctx, v);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewBigUint64(JSContext * ctx, uint64_t v, JSValue *____return) {
  JSValue ____ret = JS_NewBigUint64(ctx, v);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewBool(JSContext * ctx, int val, JSValue *____return) {
  JSValue ____ret = JS_NewBool(ctx, val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewCFunction(JSContext * ctx, JSCFunction * func, const char * name, int length, JSValue *____return) {
  JSValue ____ret = JS_NewCFunction(ctx, func, name, length);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewCFunction2(JSContext * ctx, JSCFunction * func, const char * name, int length, JSCFunctionEnum cproto, int magic, JSValue *____return) {
  JSValue ____ret = JS_NewCFunction2(ctx, func, name, length, cproto, magic);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewCFunctionData(JSContext * ctx, JSCFunctionData * func, int length, int magic, int data_len, JSValue * data, JSValue *____return) {
  JSValue ____ret = JS_NewCFunctionData(ctx, func, length, magic, data_len, data);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewCFunctionMagic(JSContext * ctx, JSCFunctionMagic * func, const char * name, int length, JSCFunctionEnum cproto, int magic, JSValue *____return) {
  JSValue ____ret = JS_NewCFunctionMagic(ctx, func, name, length, cproto, magic);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewCatchOffset(JSContext * ctx, int32_t val, JSValue *____return) {
  JSValue ____ret = JS_NewCatchOffset(ctx, val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewDate(JSContext * ctx, double epoch_ms, JSValue *____return) {
  JSValue ____ret = JS_NewDate(ctx, epoch_ms);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewError(JSContext * ctx, JSValue *____return) {
  JSValue ____ret = JS_NewError(ctx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewFloat64(JSContext * ctx, double d, JSValue *____return) {
  JSValue ____ret = JS_NewFloat64(ctx, d);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewInt32(JSContext * ctx, int32_t val, JSValue *____return) {
  JSValue ____ret = JS_NewInt32(ctx, val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewInt64(JSContext * ctx, int64_t val, JSValue *____return) {
  JSValue ____ret = JS_NewInt64(ctx, val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewObject(JSContext * ctx, JSValue *____return) {
  JSValue ____ret = JS_NewObject(ctx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewObjectClass(JSContext * ctx, int class_id, JSValue *____return) {
  JSValue ____ret = JS_NewObjectClass(ctx, class_id);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewObjectProto(JSContext * ctx, JSValue *proto, JSValue *____return) {
  JSValue ____ret = JS_NewObjectProto(ctx, *proto);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewObjectProtoClass(JSContext * ctx, JSValue *proto, JSClassID class_id, JSValue *____return) {
  JSValue ____ret = JS_NewObjectProtoClass(ctx, *proto, class_id);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewPromiseCapability(JSContext * ctx, JSValue * resolving_funcs, JSValue *____return) {
  JSValue ____ret = JS_NewPromiseCapability(ctx, resolving_funcs);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewString(JSContext * ctx, const char * str, JSValue *____return) {
  JSValue ____ret = JS_NewString(ctx, str);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewStringLen(JSContext * ctx, const char * str1, size_t len1, JSValue *____return) {
  JSValue ____ret = JS_NewStringLen(ctx, str1, len1);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewTypedArray(JSContext * ctx, int argc, JSValue * argv, JSTypedArrayEnum array_type, JSValue *____return) {
  JSValue ____ret = JS_NewTypedArray(ctx, argc, argv, array_type);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_NewUint32(JSContext * ctx, uint32_t val, JSValue *____return) {
  JSValue ____ret = JS_NewUint32(ctx, val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_ParseJSON(JSContext * ctx, const char * buf, size_t buf_len, const char * filename, JSValue *____return) {
  JSValue ____ret = JS_ParseJSON(ctx, buf, buf_len, filename);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_ParseJSON2(JSContext * ctx, const char * buf, size_t buf_len, const char * filename, int flags, JSValue *____return) {
  JSValue ____ret = JS_ParseJSON2(ctx, buf, buf_len, filename, flags);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_PreventExtensions(JSContext * ctx, JSValue *obj) {
 return JS_PreventExtensions(ctx, *obj);
};


void __sn_wrap_quickjs_JS_PromiseResult(JSContext * ctx, JSValue *promise, JSValue *____return) {
  JSValue ____ret = JS_PromiseResult(ctx, *promise);
  memcpy(____return, &____ret, sizeof(JSValue));
}


JSPromiseStateEnum __sn_wrap_quickjs_JS_PromiseState(JSContext * ctx, JSValue *promise) {
 return JS_PromiseState(ctx, *promise);
};


void __sn_wrap_quickjs_JS_ReadObject(JSContext * ctx, const uint8_t * buf, size_t buf_len, int flags, JSValue *____return) {
  JSValue ____ret = JS_ReadObject(ctx, buf, buf_len, flags);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_ResolveModule(JSContext * ctx, JSValue *obj) {
 return JS_ResolveModule(ctx, *obj);
};


int __sn_wrap_quickjs_JS_SameValue(JSContext * ctx, JSValue *op1, JSValue *op2) {
 return JS_SameValue(ctx, *op1, *op2);
};


int __sn_wrap_quickjs_JS_SameValueZero(JSContext * ctx, JSValue *op1, JSValue *op2) {
 return JS_SameValueZero(ctx, *op1, *op2);
};


void __sn_wrap_quickjs_JS_SetClassProto(JSContext * ctx, JSClassID class_id, JSValue *obj) {
 JS_SetClassProto(ctx, class_id, *obj);
};


void __sn_wrap_quickjs_JS_SetConstructor(JSContext * ctx, JSValue *func_obj, JSValue *proto) {
 JS_SetConstructor(ctx, *func_obj, *proto);
};


int __sn_wrap_quickjs_JS_SetConstructorBit(JSContext * ctx, JSValue *func_obj, int val) {
 return JS_SetConstructorBit(ctx, *func_obj, val);
};


void __sn_wrap_quickjs_JS_SetIsHTMLDDA(JSContext * ctx, JSValue *obj) {
 JS_SetIsHTMLDDA(ctx, *obj);
};


int __sn_wrap_quickjs_JS_SetModuleExport(JSContext * ctx, JSModuleDef * m, const char * export_name, JSValue *val) {
 return JS_SetModuleExport(ctx, m, export_name, *val);
};


void __sn_wrap_quickjs_JS_SetOpaque(JSValue *obj, void * opaque) {
 JS_SetOpaque(*obj, opaque);
};


int __sn_wrap_quickjs_JS_SetProperty(JSContext * ctx, JSValue *this_obj, JSAtom prop, JSValue *val) {
 return JS_SetProperty(ctx, *this_obj, prop, *val);
};


void __sn_wrap_quickjs_JS_SetPropertyFunctionList(JSContext * ctx, JSValue *obj, const JSCFunctionListEntry * tab, int len) {
 JS_SetPropertyFunctionList(ctx, *obj, tab, len);
};


int __sn_wrap_quickjs_JS_SetPropertyInt64(JSContext * ctx, JSValue *this_obj, int64_t idx, JSValue *val) {
 return JS_SetPropertyInt64(ctx, *this_obj, idx, *val);
};


int __sn_wrap_quickjs_JS_SetPropertyInternal(JSContext * ctx, JSValue *obj, JSAtom prop, JSValue *val, JSValue *this_obj, int flags) {
 return JS_SetPropertyInternal(ctx, *obj, prop, *val, *this_obj, flags);
};


int __sn_wrap_quickjs_JS_SetPropertyStr(JSContext * ctx, JSValue *this_obj, const char * prop, JSValue *val) {
 return JS_SetPropertyStr(ctx, *this_obj, prop, *val);
};


int __sn_wrap_quickjs_JS_SetPropertyUint32(JSContext * ctx, JSValue *this_obj, uint32_t idx, JSValue *val) {
 return JS_SetPropertyUint32(ctx, *this_obj, idx, *val);
};


int __sn_wrap_quickjs_JS_SetPrototype(JSContext * ctx, JSValue *obj, JSValue *proto_val) {
 return JS_SetPrototype(ctx, *obj, *proto_val);
};


void __sn_wrap_quickjs_JS_SetUncatchableError(JSContext * ctx, JSValue *val, int flag) {
 JS_SetUncatchableError(ctx, *val, flag);
};


int __sn_wrap_quickjs_JS_StrictEq(JSContext * ctx, JSValue *op1, JSValue *op2) {
 return JS_StrictEq(ctx, *op1, *op2);
};


void __sn_wrap_quickjs_JS_Throw(JSContext * ctx, JSValue *obj, JSValue *____return) {
  JSValue ____ret = JS_Throw(ctx, *obj);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_ThrowOutOfMemory(JSContext * ctx, JSValue *____return) {
  JSValue ____ret = JS_ThrowOutOfMemory(ctx);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_ToBigInt64(JSContext * ctx, int64_t * pres, JSValue *val) {
 return JS_ToBigInt64(ctx, pres, *val);
};


int __sn_wrap_quickjs_JS_ToBool(JSContext * ctx, JSValue *val) {
 return JS_ToBool(ctx, *val);
};


const char * __sn_wrap_quickjs_JS_ToCString(JSContext * ctx, JSValue *val1) {
 return JS_ToCString(ctx, *val1);
};


const char * __sn_wrap_quickjs_JS_ToCStringLen(JSContext * ctx, size_t * plen, JSValue *val1) {
 return JS_ToCStringLen(ctx, plen, *val1);
};


const char * __sn_wrap_quickjs_JS_ToCStringLen2(JSContext * ctx, size_t * plen, JSValue *val1, int cesu8) {
 return JS_ToCStringLen2(ctx, plen, *val1, cesu8);
};


int __sn_wrap_quickjs_JS_ToFloat64(JSContext * ctx, double * pres, JSValue *val) {
 return JS_ToFloat64(ctx, pres, *val);
};


int __sn_wrap_quickjs_JS_ToIndex(JSContext * ctx, uint64_t * plen, JSValue *val) {
 return JS_ToIndex(ctx, plen, *val);
};


int __sn_wrap_quickjs_JS_ToInt32(JSContext * ctx, int32_t * pres, JSValue *val) {
 return JS_ToInt32(ctx, pres, *val);
};


int __sn_wrap_quickjs_JS_ToInt64(JSContext * ctx, int64_t * pres, JSValue *val) {
 return JS_ToInt64(ctx, pres, *val);
};


int __sn_wrap_quickjs_JS_ToInt64Ext(JSContext * ctx, int64_t * pres, JSValue *val) {
 return JS_ToInt64Ext(ctx, pres, *val);
};


void __sn_wrap_quickjs_JS_ToPropertyKey(JSContext * ctx, JSValue *val, JSValue *____return) {
  JSValue ____ret = JS_ToPropertyKey(ctx, *val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_JS_ToString(JSContext * ctx, JSValue *val, JSValue *____return) {
  JSValue ____ret = JS_ToString(ctx, *val);
  memcpy(____return, &____ret, sizeof(JSValue));
}


int __sn_wrap_quickjs_JS_ToUint32(JSContext * ctx, uint32_t * pres, JSValue *val) {
 return JS_ToUint32(ctx, pres, *val);
};


int __sn_wrap_quickjs_JS_VALUE_IS_NAN(JSValue *v) {
 return JS_VALUE_IS_NAN(*v);
};


JSAtom __sn_wrap_quickjs_JS_ValueToAtom(JSContext * ctx, JSValue *val) {
 return JS_ValueToAtom(ctx, *val);
};


uint8_t * __sn_wrap_quickjs_JS_WriteObject(JSContext * ctx, size_t * psize, JSValue *obj, int flags) {
 return JS_WriteObject(ctx, psize, *obj, flags);
};


uint8_t * __sn_wrap_quickjs_JS_WriteObject2(JSContext * ctx, size_t * psize, JSValue *obj, int flags, uint8_t *** psab_tab, size_t * psab_tab_len) {
 return JS_WriteObject2(ctx, psize, *obj, flags, psab_tab, psab_tab_len);
};


void __sn_wrap_quickjs___JS_FreeValue(JSContext * ctx, JSValue *v) {
 __JS_FreeValue(ctx, *v);
};


void __sn_wrap_quickjs___JS_FreeValueRT(JSRuntime * rt, JSValue *v) {
 __JS_FreeValueRT(rt, *v);
};


void __sn_wrap_quickjs___JS_NewFloat64(JSContext * ctx, double d, JSValue *____return) {
  JSValue ____ret = __JS_NewFloat64(ctx, d);
  memcpy(____return, &____ret, sizeof(JSValue));
}


void __sn_wrap_quickjs_js_string_codePointRange(JSContext * ctx, JSValue *this_val, int argc, JSValue * argv, JSValue *____return) {
  JSValue ____ret = js_string_codePointRange(ctx, *this_val, argc, argv);
  memcpy(____return, &____ret, sizeof(JSValue));
}



