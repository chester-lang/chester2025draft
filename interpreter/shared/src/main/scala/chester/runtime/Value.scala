package chester.runtime

import spire.math.Rational

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

opaque type Value = Vector[Any] | Int | BigInt | String | Symbol | HashMap[Any, Any] | Rational | Double | Function[Any, Any]
type OpenValue = Vector[Value] | Int | BigInt | String | Symbol | HashMap[Value, Value] | Rational | Double | Function[Vector[Value], Value]

inline implicit def viewValue(x: Value): OpenValue = x.asInstanceOf[OpenValue]
