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
    from chester import chester
    
    print("\nChester module successfully imported!")
    
    # Test accessing Chester functionality
    print("\nTesting Chester functionality:")
    
    # Test the test function with a parameter
    test_input = "Hello from Python"
    test_result = chester.Chester.test(test_input)
    print(f"Chester.test('{test_input}') returned: {test_result}")
    
    # Test if the function correctly echoes back the input
    if test_result == test_input:
        print("✓ Success: Test function correctly returns the input string")
    else:
        print("✗ Error: Test function did not return the input string")
    
    # Test the helloFromJs constant
    if hasattr(chester.Chester, 'helloFromJs'):
        value = chester.Chester.helloFromJs
        print(f"Chester.helloFromJs value: {value}")
        
        if "Hello from JS" in value:
            print("✓ Success: helloFromJs constant has the expected value")
        else:
            print("✗ Error: helloFromJs constant does not have the expected value")
    else:
        print("✗ Error: helloFromJs constant not found")
    
except ImportError as e:
    print(f"Error importing Chester module: {e}")
except Exception as e:
    print(f"Error using Chester module: {e}")

print("\nTest completed.") 