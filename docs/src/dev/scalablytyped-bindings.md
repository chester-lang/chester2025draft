# ScalablyTyped Bindings Guide

This document provides information about where to find and how to use the ScalablyTyped bindings in the Chester project.

## Where to Find the Bindings

The ScalablyTyped bindings for external libraries like `ts-morph` are generated during the build process and stored in the following locations:

### Generated Source Files

The actual Scala source files for the bindings are located at:

```
js-typings/.js/target/streams/_global/stImport/_global/streams/sources/t/ts-morph/src/main/scala/typings/tsMorph/
```

Within this directory:
- `mod/` directory contains all the main classes and traits
- `anon/` directory contains anonymous types
- `tsMorphBooleans.scala`, `tsMorphInts.scala`, etc. contain constants and types

### Important Files for ts-morph

Key files for working with ts-morph:

1. `mod/SourceFile.scala` - Contains the SourceFile class definition
2. `mod/StatementedNode.scala` - Contains methods for accessing interfaces, classes, and type aliases
3. `mod/Project.scala` - Contains the Project class for creating and working with projects

## Using the Bindings in Code

There are two approaches to using ts-morph from Scala.js:

### Approach 1: Using ScalablyTyped Bindings Directly

To use the ScalablyTyped bindings in your code:

1. Import the correct namespace:
   ```scala
   import typings.tsMorph.mod
   ```

2. When working with SourceFile to access interfaces, classes, and type aliases, note that:
   - `SourceFile` does not directly extend `StatementedNode`
   - Use methods like `getInterfaces()`, `getClasses()`, and `getTypeAliases()` from the appropriate trait
   - Convert JavaScript arrays to Scala lists using `js.Array.from(...).toList`

3. Example access pattern:
   ```scala
   val project = new mod.Project()
   val sourceFile = project.addSourceFileAtPath(filePath)
   val interfaces = js.Array.from(sourceFile.getInterfaces()).toList
   ```

### Approach 2: Using Direct JavaScript Interop

For simpler integration, especially when facing type mismatches or method access issues, you can use direct JavaScript evaluation:

```scala
import scala.scalajs.js
import scala.scalajs.js.annotation.*

def analyzeTsFile(filePath: String): String = {
  // Use direct JavaScript interop with triple quotes for multiline JavaScript
  js.Dynamic.global.eval(s"""
    function analyze(filePath) {
      const { Project } = require("ts-morph");
      
      try {
        const project = new Project();
        const sourceFile = project.addSourceFileAtPath(filePath);
        
        // Use native JavaScript APIs directly
        const interfaces = sourceFile.getInterfaces().map(interface => ({
          name: interface.getName(),
          // ... other properties
        }));
        
        return JSON.stringify(interfaces);
      } catch (e) {
        return JSON.stringify({ error: e.message });
      }
    }
    
    analyze("${filePath}");
  """).asInstanceOf[String]
}
```

This approach:
- Avoids type mismatches and casting issues
- Uses native JavaScript directly 
- Returns results as JSON strings
- Escapes Scala-JavaScript interpolation issues using triple quotes

## Common Gotchas

1. **Array Conversion**: JavaScript arrays need to be converted to Scala collections using `js.Array.from(...).toList`

2. **Method Names**: Some method names in the bindings may differ from the original TypeScript API

3. **Inheritance Hierarchy**: The inheritance hierarchy in the bindings may not match the TypeScript original exactly

4. **Type Conversion**: Sometimes explicit type conversion is needed when working with the bindings

5. **String Interpolation**: When using direct JavaScript eval, use Scala triple quotes (`"""`) and properly escape JavaScript template literals (`${...}` to `$${...}`)

## Rebuilding Bindings

If you need to rebuild the ScalablyTyped bindings:

1. Run `sbt "jsTypings/stImport"` to regenerate the bindings
2. For troubleshooting binding generation, check `jsTypings/stOutputs` 