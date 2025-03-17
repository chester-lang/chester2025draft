# JavaScript to Python Conversion Process

## Overview

This document outlines the process for converting JavaScript code generated from Scala.js into Python-accessible modules. This approach allows Chester functionality written in Scala to be available within Python environments.

## Process Flow

The conversion process follows these steps:

1. **Compile Scala.js to JavaScript** - Use the `sbt jsForPython/fastLinkJS` SBT task to compile Scala code to JavaScript
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
sbt jsForPython/fastLinkJS
```

This produces JavaScript files in the `js-for-python/js/target/` directory.

### 2. Bundling with Rollup

The `rollup.config.mjs` file defines how to bundle the generated JavaScript:

```javascript
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
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
    })
  ],
};
```

To bundle the JavaScript code, create an `index.js` file that imports the generated `.mjs` files, then run:

```bash
pnpm install  # Only need to run this once to install dependencies
pnpm run build
```

This produces a bundled JavaScript file at `dist/bundle.js`.

### 3. JavaScript to Python with js2py

#### Python Environment Setup with UV

We use the `uv` package manager for Python dependencies due to its improved performance and reliability. All Python-related work should be done in the `js-for-python` directory, which contains a `.python-version` file specifying Python 3.11:

```bash
# Navigate to the js-for-python directory
cd js-for-python

# Create a virtual environment with the specified Python version
uv venv -p 3.11

# Activate the virtual environment
source .venv/bin/activate  # On Unix/macOS
# or
# .venv\Scripts\activate  # On Windows

# Install dependencies using requirements.txt
uv pip install -r requirements.txt
```

#### Automated Translation Process

The `js2py_build.py` script in the `python` directory automates the translation process:

```python
# Usage
python python/js2py_build.py  # Translates the bundle.js to chester.py
python python/js2py_build.py --force  # Forces retranslation even if chester.py exists
```

This script performs the following steps:
1. Verifies the bundle.js file exists
2. Preprocesses the JavaScript to handle js2py compatibility issues
3. Translates the JavaScript to Python using js2py.translate_file()
4. Outputs the result to `python/chester.py`

## Usage Guidelines

1. Expose Scala functions using `@JSExportTopLevel`:
   ```scala
   @JSExportTopLevel("functionName")
   def functionName(param: Type): ReturnType = {
     // Implementation
   }
   ```

2. Bundle only what's necessary to minimize final bundle size.

3. Access the Chester functionality from Python:

   ```python
   # Import the Chester module
   from chester import chester
   
   # Access functions via the Chester global object
   result = chester.Chester.test()
   ```

## Testing

The project includes two test scripts:

### 1. test_js2py.py 

Tests basic js2py functionality with a simple JavaScript example. It:
- Translates example.js to Python
- Imports and uses the translated module
- Tests various js2py features
- Tests the Chester JS -> Python bridge

To run:
```bash
python python/test_js2py.py
```

### 2. test_chester.py

Tests the generated Chester Python module. It:
- Checks if the chester.py module exists and generates it if needed
- Imports the module and tests available functions
- Reports any errors

To run:
```bash
python python/test_chester.py
```

## Complete Testing Sequence

To fully test the JavaScript to Python conversion:

```bash
# 1. Compile Scala.js to JavaScript
sbt jsForPython/fastLinkJS

# 2. Bundle with Rollup
cd js-for-python
pnpm install  # First time only
pnpm run build

# 3. Set up Python environment
uv venv -p 3.11
source .venv/bin/activate
uv pip install -r requirements.txt

# 4. Test js2py and simple JavaScript
python python/test_js2py.py

# 5. Test Chester module
python python/test_chester.py
```

## Project Structure

Current project structure:

```
js-for-python/
├── js/                          # Scala.js source files
│   └── src/main/scala/chester/
├── python/                      # Python integration
│   ├── js2py_build.py           # Script to translate bundle.js to chester.py 
│   ├── test_js2py.py            # Script for testing js2py functionality
│   ├── test_chester.py          # Script for testing Chester module
│   └── chester.py               # Generated Python module (after build)
├── dist/                        # Bundled JavaScript output
│   └── bundle.js                # Generated JavaScript bundle (after build)
├── index.js                     # Entry point for rollup
├── package.json                 # Node.js package configuration
├── rollup.config.mjs            # Rollup configuration
├── .python-version              # Specifies Python 3.11
└── requirements.txt             # Python dependencies
```

## Troubleshooting

- **CommonJS vs ESM**: Ensure module formats are compatible between Scala.js output and Rollup configuration.
- **js2py limitations**: js2py has limited ECMAScript compatibility; avoid advanced JS features.
- **Bundle size**: Large bundles may impact Python startup time; optimize bundle size when possible.
- **Python version compatibility**: js2py works best with Python 3.8-3.11. We're currently using Python 3.11.
- **Special character handling**: js2py doesn't support functions with special characters in their names (like `$`) when accessing them directly. Use `getattr()` instead:
  ```python
  # Instead of: module.$function()
  getattr(module, "$function")()
  ```
- **Object serialization issues**: When encountering "Cannot convert object to primitive value" errors, explicitly use string conversion:
  ```javascript
  // Instead of: "text" + object
  "text" + String(object)
  ```
