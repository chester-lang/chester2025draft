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

# js-for-lua

Convert Scala.js to Lua using CASTL.

## Overview

This package converts Scala.js output to Lua through a multi-step process:

1. Compile the Scala.js code
2. Bundle the JS code using rollup
3. Convert the JS bundle to Lua using CASTL
4. Create a self-contained Lua bundle

## Installation

### Prerequisites

- Node.js (v14+)
- npm or pnpm
- Scala.js project compiled to JavaScript

### Install Dependencies

```bash
npm install
# or 
pnpm install
```

## Usage

### Full Build Process

To run the complete build process (JS bundling, conversion to Lua, and creating self-contained bundle):

```bash
npm run build-lua
# or
pnpm run build-lua
```

This will:
1. Bundle the JavaScript using rollup
2. Convert the JS bundle to Lua using CASTL
3. Create a self-contained Lua bundle with all dependencies

### Individual Steps

You can also run each step individually:

```bash
# Step 1: Bundle JavaScript with rollup
npm run build

# Step 2: Convert the JS bundle to Lua
npm run js2lua

# Step 3: Create self-contained Lua bundle
npm run create-bundle
```

## Output Files

After running the build process, you'll find the following files in the `lua` directory:

- `chester.lua` - The main Lua file converted from JavaScript
- `castl/` - Directory containing CASTL runtime files
- `chester.bundle.lua` - A self-contained Lua file with CASTL runtime
- `chester_loader.lua` - A helper script to load the bundle in other projects
- `test_chester.lua` - A test script for the non-bundled version
- `test_bundle.lua` - A test script for the bundled version

## Using the Bundled Lua Output

### Option 1: Direct Use

The bundled output (`chester.bundle.lua`) is a self-contained file that includes all dependencies:

```lua
-- In your Lua project
local chester = require("chester.bundle")

-- Use chester functions
print(chester.test())
```

### Option 2: Using the Loader

For simpler integration, you can use the provided loader script:

```lua
-- First, require the loader (adjust path as needed)
require("path.to.chester_loader")

-- Then use the chester global directly
print(chester.test())
```

## Development

### Project Structure

- `index.js` - Entry point for the JavaScript module
- `js2lua.js` - Script for converting JavaScript to Lua
- `lua-bundler.js` - Script for creating self-contained Lua bundle
- `rollup.config.mjs` - Rollup configuration for bundling JavaScript
- `dist/` - Directory containing the bundled JavaScript
- `lua/` - Directory containing the converted Lua files

### How the Bundle Works

The self-contained bundle (`chester.bundle.lua`):

1. Includes the CASTL runtime in a local scope
2. Initializes the runtime environment
3. Loads the Chester module code
4. Makes the Chester module available through Lua's module system

This approach eliminates the need for external dependencies and complex module resolution.

## Current Issues and Limitations

### Runtime Dependencies

1. **Bit Operations Library**
   - The CASTL runtime requires bit operations support
   - Currently trying to use Lua 5.4's bit32 library
   - Need to install bit32 via LuaRocks
   - Alternative: Consider using a pure Lua implementation of bit operations

2. **Module Resolution**
   - Some issues with module resolution in the bundled version
   - Need to ensure proper loading order of runtime and module files
   - May need to adjust package.path settings

### Next Steps

1. **Runtime Dependencies**
   - [ ] Install and test bit32 library
   - [ ] Consider alternative bit operation implementations
   - [ ] Document all runtime dependencies clearly

2. **Module System**
   - [ ] Fix module resolution issues
   - [ ] Improve error handling for missing dependencies
   - [ ] Add better module loading diagnostics

3. **Testing**
   - [ ] Set up proper test environment
   - [ ] Add more comprehensive test cases
   - [ ] Add CI/CD pipeline for testing

## License

See the project repository for license information. 