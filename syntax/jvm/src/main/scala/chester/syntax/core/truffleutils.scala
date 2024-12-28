package chester.syntax.core

import com.oracle.truffle.api.nodes.Node.Child

import scala.annotation.meta.field

// https://github.com/b-studios/scala-graal-truffle-example/blob/c2747a6eece156f878c5b934116aaa00a2cd6311/src/main/scala/effekt/Test.scala#L79
// We define our own alias, using Scala's meta-annotation
// (https://www.scala-lang.org/api/current/scala/annotation/meta/index.html)
// This is necessary to make sure the *field* is annotated, not the getters
// or setters.
type child = Child @field
