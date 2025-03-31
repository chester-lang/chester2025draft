import { promises as fs } from 'fs';
import { execSync } from 'child_process';
import path from 'path';

/**
 * JavaScript to Lua conversion using CASTL
 */
async function convertJsToLua() {
  try {
    // Check if bundle.js exists
    const bundlePath = path.join(process.cwd(), 'dist', 'bundle.js');
    await fs.access(bundlePath);
    
    // Create lua output directory if it doesn't exist
    const luaDir = path.join(process.cwd(), 'lua');
    try {
      await fs.mkdir(luaDir, { recursive: true });
    } catch (err) {
      if (err.code !== 'EEXIST') throw err;
    }
    
    // Output path for the Lua file
    const outputPath = path.join(luaDir, 'chester.lua');
    
    console.log('Converting JavaScript to Lua using CASTL...');
    // Use CASTL to convert the bundle.js to Lua
    // The -o flag outputs the Lua code to a file with the same name but .lua extension
    // The --babel flag ensures ES6+ compatibility
    // The --jit flag optimizes for LuaJIT if needed
    execSync(`npx castl ${bundlePath} -o ${outputPath} --babel`, {
      stdio: 'inherit'
    });
    
    console.log(`JavaScript successfully converted to Lua: ${outputPath}`);
    
    // Copy the CASTL runtime library to the lua directory
    console.log('Copying CASTL runtime library...');
    const castlRuntimeDir = path.join(process.cwd(), 'node_modules', 'castl', 'lua', 'castl');
    const targetRuntimeDir = path.join(luaDir, 'castl');
    
    try {
      await fs.mkdir(targetRuntimeDir, { recursive: true });
    } catch (err) {
      if (err.code !== 'EEXIST') throw err;
    }
    
    // Copy runtime files
    const files = await fs.readdir(castlRuntimeDir);
    for (const file of files) {
      const srcPath = path.join(castlRuntimeDir, file);
      const destPath = path.join(targetRuntimeDir, file);
      const stat = await fs.stat(srcPath);
      
      if (stat.isDirectory()) {
        // For directories like 'jscompile', copy recursively
        execSync(`cp -r "${srcPath}" "${targetRuntimeDir}"`, { stdio: 'inherit' });
      } else {
        // For regular files
        await fs.copyFile(srcPath, destPath);
      }
    }
    
    console.log('CASTL runtime library copied successfully');
    
    // Create a simple test script
    const testScriptPath = path.join(luaDir, 'test_chester.lua');
    const testScriptContent = `
-- Load CASTL runtime
package.path = package.path .. ";./?.lua;./?/init.lua"
require('castl.runtime')

-- Load the Chester module
local chester = require('chester')

-- Test the module
print("Testing Chester module...")
-- Add actual tests based on your exposed functions
-- Example: print(chester.test())
`;
    
    await fs.writeFile(testScriptPath, testScriptContent);
    console.log(`Test script created: ${testScriptPath}`);
    
  } catch (error) {
    console.error('Error during conversion:', error);
    process.exit(1);
  }
}

// Run the conversion
convertJsToLua(); 