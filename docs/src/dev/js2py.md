# JavaScript to Python Conversion Process

## Overview

This document outlines the process for converting JavaScript code generated from Scala.js into Python-accessible modules. This approach allows Chester functionality written in Scala to be available within Python environments.

## Process Flow

The conversion process follows these steps:

1. **Compile Scala.js to JavaScript** - Use the `jsForPython/fullLinkJS` SBT task to compile Scala code to JavaScript
2. **Bundle with Rollup** - Use Rollup to combine the generated JavaScript with any needed glue code into a single module
3. **Convert to Python** - Use js2py to make the JavaScript functionality accessible from Python

## Step-by-Step Implementation

### 1. Scala.js Compilation

The `jsForPython` project in `build.sbt` is configured to compile Scala code to ECMAScript modules with the `.mjs` extension:

```scala
lazy val jsForPython = crossProject(JSPlatform)
  .withoutSuffixFor(JSPlatform)
  .crossType(CrossType.Full)
  .in(file("js-for-python"))
  .settings(
    commonSettings,
    name := "js-for-python"
  )
  .jsConfigure(_.dependsOn(utils.js))
  .jsSettings(
    scalaJSLinkerConfig ~= {
      // Enable ECMAScript module output.
      _.withModuleKind(ModuleKind.ESModule)
        // Use .mjs extension.
        .withOutputPatterns(OutputPatterns.fromJSFile("%s.mjs"))
    }
  )
```

To compile the Scala.js code, run:

```bash
sbt jsForPython/fullLinkJS
```

This produces JavaScript files in the `js-for-python/js/target/` directory.

### 2. Bundling with Rollup

The `rollup.config.mjs` file defines how to bundle the generated JavaScript:

```javascript
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';
import babel from '@rollup/plugin-babel';

export default {
  input: 'index.js',
  output: {
    file: 'dist/bundle.js',
    format: 'cjs',
    sourcemap: true,
  },
  plugins: [
    resolve({
      preferBuiltins: false,
    }),
    commonjs(),
    babel({
      babelHelpers: 'bundled',
      presets: [
        ['@babel/preset-env', { targets: { node: "18" } }]
      ]
    }),
    terser({
      compress: {
        dead_code: true,
      },
    }),
  ],
};
```

To bundle the JavaScript code, create an `index.js` file that imports the generated `.mjs` files, then run:

```bash
pnpm run build
```

This produces a bundled JavaScript file at `dist/bundle.js`.

### 3. JavaScript to Python with js2py

The `test_js2py_bridge.py` file demonstrates how to use js2py to integrate JavaScript code with Python:

```python
from Js2PyBridge import Js2PyBridge

def main():
    bridge = Js2PyBridge()
    
    # Evaluate JavaScript code
    bridge.eval_js(js_code)
    
    # Get variables from JavaScript
    result = bridge.get_value("testVar")
    
    # Call JavaScript functions
    result = bridge.call_function("testFunction", "World")
```

## Usage Guidelines

1. Expose Scala functions using `@JSExportTopLevel`:
   ```scala
   @JSExportTopLevel("functionName")
   def functionName(param: Type): ReturnType = {
     // Implementation
   }
   ```

2. Bundle only what's necessary to minimize final bundle size.

3. Consider creating a Python package wrapper for the js2py bridge for easier distribution.

## Testing

Always test the full pipeline:

1. Verify Scala.js compilation produces the expected JavaScript.
2. Confirm that Rollup successfully bundles the code.
3. Test that js2py can import and use the bundled JavaScript.
4. Write Python unit tests for the exposed functionality.

## Troubleshooting

- **CommonJS vs ESM**: Ensure module formats are compatible between Scala.js output and Rollup configuration.
- **js2py limitations**: js2py has limited ECMAScript compatibility; avoid advanced JS features.
- **Bundle size**: Large bundles may impact Python startup time; optimize bundle size when possible.
