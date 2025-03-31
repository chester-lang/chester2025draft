# Scala to Lua Conversion System

This project enables Chester functionality written in Scala to be accessible from Lua environments. It uses a combination of Scala.js (to compile Scala to JavaScript) and CASTL (to convert JavaScript to Lua).

## Overview

The conversion process follows these steps:

1. **Compile Scala to JavaScript** - Using Scala.js to compile Scala code to JavaScript
2. **Bundle JavaScript** - Using Rollup to bundle the JavaScript code
3. **Convert to Lua** - Using CASTL to convert JavaScript to Lua
4. **Runtime Support** - Providing necessary runtime libraries for Lua execution

## Prerequisites

- Scala/SBT (for compiling Scala.js code)
- Node.js/npm (for JavaScript bundling and CASTL)
- Lua 5.2 or LuaJIT 2 (for running the Lua code)
- PCRE library for Lua (for regular expression support)

### Installing PCRE for Lua

CASTL requires the Lrexlib-pcre library for handling regular expressions:

```bash
# For Lua 5.2
sudo luarocks-5.2 install lrexlib-pcre

# For LuaJIT
sudo luarocks-5.1 install lrexlib-pcre
```

You'll also need libpcre installed on your system:

```bash
# On Ubuntu/Debian
sudo apt-get install libpcre3 libpcre3-dev

# On macOS with Homebrew
brew install pcre
```

## Building

### 1. Compile Scala.js to JavaScript

```bash
# From the project root
sbt jsForLua/fastLinkJS
```

This produces JavaScript files in the `js-for-lua/js/target/` directory.

### 2. Bundle with Rollup and Convert to Lua

```bash
# Navigate to the js-for-lua directory
cd js-for-lua

# Install dependencies
npm install

# Bundle JavaScript and convert to Lua
npm run build
npm run js2lua
```

This produces a Lua file at `js-for-lua/lua/chester.lua` and copies the necessary CASTL runtime.

## Usage

### In a Lua Script

```lua
-- Add the js-for-lua/lua directory to the Lua path
package.path = package.path .. ";path/to/js-for-lua/lua/?.lua;path/to/js-for-lua/lua/?/init.lua"

-- Load the CASTL runtime
require('castl.runtime')

-- Load the Chester module
local chester = require('chester')

-- Use the Chester functionality
print(chester.Chester.test())
print(chester.Chester.factorial(5))
```

### Running the Test Script

```bash
cd js-for-lua/lua
lua test_chester.lua
```

## Extending

To add new functionality:

1. Add new methods in `js-for-lua/js/src/main/scala/chester/LuaExports.scala`
2. Export the functions using `@JSExportTopLevel`
3. Add the functions to the `chester` object for easy access
4. Rebuild the project following the build steps above

## Troubleshooting

### Common Issues

- **Missing CASTL runtime**: Make sure the CASTL runtime is properly installed and in the Lua path
- **Regular expression errors**: Verify that lrexlib-pcre is properly installed
- **Module not found**: Check that your Lua path includes the directory with chester.lua

### Debugging

For more detailed information during conversion:

```bash
# Run CASTL with verbose flag
npx castl dist/bundle.js -o lua/chester.lua --babel -v
```

## Limitations

- Unlike js2py which faithfully translates JavaScript to Python, CASTL may have limitations with certain JavaScript features
- Complex JS language features may not be fully supported
- JavaScript's object model doesn't match Lua's table-based approach perfectly
- Some native JS objects and methods may not be available

## Future Improvements

- Add support for more complex data structures
- Improve error handling between Scala and Lua
- Create a more comprehensive test suite
- Add automated build pipeline 