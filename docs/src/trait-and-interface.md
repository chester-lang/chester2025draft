# Traits and Interfaces in Chester

Chester provides two distinct mechanisms for defining abstract types: traits and interfaces. While both serve to define contracts that other types can implement, they differ in their subtyping behavior and intended use cases.

## Traits: Nominal Subtyping

Traits in Chester use nominal subtyping, which means that the name of the type is significant in determining subtype relationships.

Here's an example of a trait definition in Chester:

```chester,playground,editable
trait Animal {
  def makeSound: String
}

object Dog <: Animal {
  override def makeSound: String = "Woof!"
}
```

In this example, `Dog` is explicitly declared as a subtype of `Animal` using the `<:` operator. This relationship is based on the names of the types, not just their structure.

## Interfaces: Structural Subtyping

Interfaces in Chester use structural subtyping, which means that subtype relationships are determined by the structure (methods and properties) of the types, regardless of their names.

Here's an example of an interface definition:

```chester,playground,editable
interface Soundmaker {
  def makeSound: String
}

object Cat {
  def makeSound: String = "Meow!"
}

// Cat is implicitly a Soundmaker because it has a matching structure
def soundmaker: Soundmaker = Cat
```

In this case, `Cat` is considered a subtype of `Soundmaker` because it has a matching `makeSound` method, even though it wasn't explicitly declared as such.

## Design Inspiration

This dual approach to abstract types is inspired by the Pony programming language, as described in:

Steed, G., & Drossopoulou, S. (2016). A principled design of capabilities in Pony. URL: https://www.ponylang.io/media/papers/a_prinicipled_design_of_capabilities_in_pony.pdf

The Pony language uses a similar distinction between traits (which they call "interfaces") for nominal subtyping and "structural types" for structural subtyping.

## Choosing Between Traits and Interfaces

- Use traits when you want to create a named hierarchy of types and control which types can be subtypes.
- Use interfaces when you want to define a contract that any type can implicitly satisfy by implementing the required structure.

This design gives Chester developers flexibility in how they structure their code, allowing for both strict hierarchies and more flexible, duck-typed-style programming where appropriate.