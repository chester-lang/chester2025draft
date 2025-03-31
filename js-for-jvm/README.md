# js-for-jvm

This module provides JavaScript functionality to JVM-based applications using Mozilla Rhino as the JavaScript engine.

## TypeScript Analysis

The module includes TypeScript analysis capabilities using ts-morph.

### Features

- Parse TypeScript source code from strings or files
- Extract interface and class definitions
- Analyze property types, including optional properties
- Get property types and metadata
- Full TypeScript AST traversal capabilities
- Bidirectional data transfer using upickle for serialization

### Usage Example

```scala
import chester.TsMorph

// Parse TypeScript directly from source
val tsCode = """
  export interface User {
    id: number;
    name: string;
    email?: string;
  }
"""

TsMorph.parseTypeScript(tsCode, "example.ts") match {
  case Right(types) => 
    // Process the extracted types
    types.foreach { typeInfo =>
      println(s"Found ${typeInfo.kind}: ${typeInfo.name}")
      
      // Access properties
      typeInfo.properties.foreach { prop =>
        val optionalMark = if (prop.isOptional) "?" else ""
        println(s"  - ${prop.name}$optionalMark: ${prop.typeText}")
      }
    }
    
  case Left(error) =>
    println(s"Error parsing TypeScript: $error")
}

// Or analyze an existing TypeScript file
TsMorph.analyzeTsFile("path/to/file.ts") match {
  case Right(types) => 
    // Process the extracted types
    
  case Left(error) =>
    println(s"Error analyzing TypeScript file: $error")
}
```

### How It Works

1. TypeScript code is parsed using the ts-morph library in the JavaScript environment
2. The parsed AST is analyzed to extract type information
3. The information is serialized to JSON using upickle
4. The JVM side deserializes the JSON back into Scala case classes
5. The resulting data structure is returned to the caller

### Model Classes

The module provides the following model classes for working with TypeScript types:

- `TypeInfo` - Contains information about a TypeScript type (interface, class, type alias)
- `PropertyInfo` - Contains information about a property in a type
- `Location` - Contains the file path, line, and column of a type

## Development

To build the module:

1. Make changes to the Scala.js files in the `js/src/main/scala/chester/js4jvm` directory
2. Run `sbt jsForJvm/js/fastLinkJS` to compile the Scala.js code to JavaScript
3. Run `cd js-for-jvm && pnpm install && pnpm run build` to bundle the JavaScript code
4. Run tests with `sbt "rootJVM/testOnly chester.TsMorphTest"`

## Requirements

- ts-morph 25.0.1+
- upickle 4.1.0+ 