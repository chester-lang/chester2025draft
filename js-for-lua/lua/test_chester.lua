
-- Load CASTL runtime
package.path = package.path .. ";./?.lua;./?/init.lua"
require('castl.runtime')

-- Load the Chester module
local chester = require('chester')

-- Test the module
print("Testing Chester module...")
-- Add actual tests based on your exposed functions
-- Example: print(chester.test())
