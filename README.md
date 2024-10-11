# chester-lang

This project is under development and not usable yet.

Warning: This project does contain very low quality large language model generated code when the person is too tired to code.

https://www.the-lingo.org - note that the domain is subject to change

[VSCode Extension](https://marketplace.visualstudio.com/items?itemName=mio-19.chester-language-support)

## Get Started

1. Install [Proto](https://moonrepo.dev/docs/proto/install) to manage Chester versions.

Linux, macOS, WSL

```bash
curl -fsSL https://moonrepo.dev/install/proto.sh | bash
```

Windows

```powershell
irm https://moonrepo.dev/install/proto.ps1 | iex
```

2. Use Proto to install Chester.

```bash
proto plugin add --global chester "https://github.com/chester-lang/chester/raw/refs/heads/main/proto.toml"
proto install chester
```

## Build requirements

+ Node.JS and PNPM
+ SBT
+ JDK 11 +
+ [Optional] GraalVM
+ [Optional] Clang

## Example

It might look like?

```chester
module 🐰;

trait 舞 <: Show;

record 超会議 <: 舞 derives Show {
  val year: Nat;
}

object InternetOverdose <: 舞 derives Show;

module 超会議 {
  val バタフライ_グラフィティ: 舞 = 超会議(2017);
}
i: InternetOverdose.type = InternetOverdose;

sealed trait Expr[T: Type] {
  def eval: T;
}

record IVal <: Expr[Int] {
  val val: Int;
  override def eval = val;
}

record BVal <: Expr[Int] {
  val val: Bool;
  override def eval = val;
}

sealed trait Vect[n: Nat, T: Type] {
  def apply(index: Fin n): T = ?todo;
}

object Nil[T] <: Vect[0, T];
record Cons[n,T] <: Vect[n+1, T] {
  val head: T;
  val tail: Vect[n, T];
}
n: Nil.type[Int] = Nil;

proof1[T]: Nil[T] = Nil[T];
proof1[T] = ?hole;

// alternative syntax for GADT
data Vect2[n: Nat, T: Type] {
  case Nil[T] <: Vect[0, T];
  case Cons[n,T](head: T, tail: Vect[n, T]) <: Vect[n+1, T];
}

record MutableString {
  var name: String;
}

record MutableStringExplicit[a: Region] {
  var[a] name: String;
}

record Box[a] {
  var var: a;
}

record BoxExplicit[a][s: Region] {
  var[s] var: a;
}

// an effect for global variables?
module MutModule / Global {
  let a = Box(0);
}

// IO somehow gives an implicit Region?
entry: Unit / IO = {
  let a = MutableString("");
  a.name = "はっぱ - もうすぐ楽になるからね";
  println(a.name);
}

AnonymousRecordType: Type = {name: String};
record1: AnonymousRecordType = {
  name = "Ame-chan";
}

extension [T](xs: List[T]) {
  // & denotes a second class type that won't leave the scope. Nonlocal returns and additional algebraic effects can be used in a second class function. original idea: Brachthäuser and Schuster (2017)
  def map[U](f: & T -> U): List[U] = ?todo;
  // def map[U,e](f: T -> U / e): List[U] / e = ?todo;
}
```

## Reference

```
Brachthäuser, J. I., & Schuster, P. (2017). Effekt: Extensible algebraic effects in Scala (short paper). Proceedings of the 8th ACM SIGPLAN International Symposium on Scala (SCALA 2017), 67–72. https://doi.org/10.1145/3136000.3136007
```
