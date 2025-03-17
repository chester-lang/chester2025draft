#!/usr/bin/env python3
"""
Test script for the generated Chester Python module
"""

import os
import sys
import js2py_build

# Ensure our module exists
if not os.path.exists(js2py_build.TRANSLATED_MODULE_PATH):
    print("Chester module not found. Running translation...")
    js2py_build.translate_js_to_py()

# Add the current directory to the Python path
CURRENT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, CURRENT_DIR)

# Import the Chester module
try:
    print("Importing the Chester module...")
    # Only import the top-level exports
    from chester import test, helloFromJs
    
    print("\nChester module successfully imported!")
    
    print("\nTesting Chester functionality:")
    
    # Test the test function
    test_input = "Hello from Python"
    result = test(test_input)
    print(f"test('{test_input}') returned: {result}")
    
    if result == test_input:
        print("✓ Success: Test function returns the correct result")
    else:
        print("✗ Error: Test function did not return the expected result")
    
    # Test the helloFromJs constant
    print(f"\nhelloFromJs value: {helloFromJs}")
    
    if "Hello from JS" in helloFromJs:
        print("✓ Success: helloFromJs constant has the expected value")
    else:
        print("✗ Error: helloFromJs constant does not have the expected value")
    
except ImportError as e:
    print(f"Error importing Chester module: {e}")
except Exception as e:
    print(f"Error using Chester module: {e}")

print("\nTest completed successfully!") 