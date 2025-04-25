import { promises as fs } from 'fs';
import path from 'path';

/**
 * Creates a self-contained Lua bundle
 */
async function createLuaBundle() {
  try {
    const luaDir = path.join(process.cwd(), 'lua');
    const chesterLuaPath = path.join(luaDir, 'chester.lua');
    const bundlePath = path.join(luaDir, 'chester.bundle.lua');
    
    // Make sure lua directory exists
    try {
      await fs.access(luaDir);
    } catch (err) {
      console.error('Lua directory does not exist. Run js2lua first.');
      process.exit(1);
    }
    
    // Make sure chester.lua exists
    try {
      await fs.access(chesterLuaPath);
    } catch (err) {
      console.error('chester.lua does not exist. Run js2lua first.');
      process.exit(1);
    }
    
    console.log('Creating Lua bundle files...');
    
    // Read the main runtime file
    const runtimePath = path.join(luaDir, 'castl', 'runtime.lua');
    const runtimeContent = await fs.readFile(runtimePath, 'utf8');
    
    // Read the chester.lua file
    const chesterContent = await fs.readFile(chesterLuaPath, 'utf8');
    
    // Create the runtime file
    const runtimeBundlePath = path.join(luaDir, 'chester.runtime.lua');
    const runtimeBundleContent = `--[[ Chester Runtime Bundle ]]--
-- This file contains the CASTL runtime

-- Define package.loaded table if it doesn't exist
package = package or {}
package.loaded = package.loaded or {}

-- CASTL Runtime
do
${runtimeContent}
end

-- Initialize CASTL runtime
_G.castl_runtime = castl_runtime or castl.runtime
`;
    
    // Create the module file
    const moduleBundlePath = path.join(luaDir, 'chester.module.lua');
    const moduleBundleContent = `--[[ Chester Module Bundle ]]--
-- This file contains the Chester module implementation

-- Load runtime if not already loaded
if not _G.castl_runtime then
  local runtime_path = debug.getinfo(1, "S").source:sub(2):match("(.*/)") or "./"
  dofile(runtime_path .. "chester.runtime.lua")
end

-- Chester Module Implementation
local chester = (function()
${chesterContent}
end)()

-- Export the chester module
package.loaded["chester"] = chester
return chester
`;
    
    // Write the bundle files
    await fs.writeFile(runtimeBundlePath, runtimeBundleContent);
    await fs.writeFile(moduleBundlePath, moduleBundleContent);
    console.log(`Runtime bundle created: ${runtimeBundlePath}`);
    console.log(`Module bundle created: ${moduleBundlePath}`);
    
    // Create a bundled test script
    const testBundlePath = path.join(luaDir, 'test_bundle.lua');
    const testBundleContent = `--[[ Chester Bundle Test ]]--
-- This script tests the bundled Chester module

-- Load the runtime and module
dofile("chester.runtime.lua")
local chester = dofile("chester.module.lua")

-- Test the module
print("Testing bundled Chester module...")
print("chester.test() result:", chester.test())
print("chester.reverseString('Hello Lua') result:", chester.reverseString("Hello Lua"))
print("chester.factorial(5) result:", chester.factorial(5))
print("chester.processData({name='test', value=42}) result:")
local result = chester.processData({name="test", value=42})
for k, v in pairs(result) do
  print("  " .. k .. ": " .. tostring(v))
end
`;
    
    await fs.writeFile(testBundlePath, testBundleContent);
    console.log(`Test script created: ${testBundlePath}`);
    
    // Create a simple loader that can be used in other Lua environments
    const loaderPath = path.join(luaDir, 'chester_loader.lua');
    const loaderContent = `--[[ Chester Loader ]]--
-- This is a simple loader that sets up the environment and loads the chester module

-- Usage in your Lua projects:
-- require("path.to.chester_loader")
-- local result = chester.someFunction()

-- Load the runtime and module
local loader_path = debug.getinfo(1, "S").source:sub(2):match("(.*/)") or "./"
dofile(loader_path .. "chester.runtime.lua")
local chester = dofile(loader_path .. "chester.module.lua")

-- Make the chester module globally available
_G.chester = chester
print("Chester Lua module loaded successfully!")
`;
    
    await fs.writeFile(loaderPath, loaderContent);
    console.log(`Loader script created: ${loaderPath}`);
    
  } catch (error) {
    console.error('Error during bundle creation:', error);
    process.exit(1);
  }
}

// Run the bundle creation
createLuaBundle(); 