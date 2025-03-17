import os
import sys
import js2py
import js2py_build

# Get paths
CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_DIR = os.path.dirname(CURRENT_DIR)
EXAMPLE_JS_PATH = os.path.join(PROJECT_DIR, 'example.js')
EXAMPLE_PY_PATH = os.path.join(CURRENT_DIR, 'example.py')

print("=== Testing js2py with example.js ===")

# Translate the JavaScript file to Python
print("\nTranslating example.js to example.py...")
js2py.translate_file(EXAMPLE_JS_PATH, EXAMPLE_PY_PATH)
print("Translation complete!")

# Import and use the translated module
print("\nImporting and using the translated module:")
sys.path.insert(0, CURRENT_DIR)
from example import example

# Access variables and objects
print("\nAccessing variables and objects:")
print("someVariable:", example.someVariable.to_dict())
print("x is a Rectangle with width:", example.x.w, "and height:", example.x.h)
print("x.getArea():", example.x.getArea())

# Call functions
print("\nCalling functions:")
example.sayHello("World")
# Using getattr to access function with special characters in name
getattr(example, "$nonPyName")()

# Direct JS evaluation
print("\nDirect JS evaluation:")
context = js2py.EvalJs()
context.execute('''
var a = 10;
function square(x) { return x * x; }
''')
print("a:", context.a)
print("square(5):", context.square(5))

# Using Python functions in JavaScript
print("\nUsing Python functions in JavaScript:")
import math
context = js2py.EvalJs({'py_sqrt': math.sqrt})
context.execute('''
var result = py_sqrt(16);
console.log("Square root of 16 is: " + result);
''')

print("\n=== Testing js2py_build (Chester JS -> Python bridge) ===")

# Verify if the bundle file exists
bundle_path = js2py_build.BUNDLE_PATH
print(f"\nChecking for bundle file at: {bundle_path}")
if os.path.exists(bundle_path):
    print("Bundle file exists!")
    try:
        print("\nRunning js2py_build.translate_js_to_py()...")
        success = js2py_build.translate_js_to_py(force=True)
        print(f"Translation {'succeeded' if success else 'failed'}")
        
        if success and os.path.exists(js2py_build.TRANSLATED_MODULE_PATH):
            print(f"\nTranslated module exists at: {js2py_build.TRANSLATED_MODULE_PATH}")
            print("Module size:", os.path.getsize(js2py_build.TRANSLATED_MODULE_PATH), "bytes")
        else:
            print("Translated module was not created")
    except Exception as e:
        print(f"Error during translation: {e}")
else:
    print("Bundle file does not exist. Please run 'pnpm run build' first.")
    
print("\nTest completed successfully!") 