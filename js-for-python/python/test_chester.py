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
    
    # Test the test function
    test_result = chester.Chester.test()
    print(f"Chester.test() returned: {test_result}")
    
    # Test other available functions
    if hasattr(chester.Chester, 'originalTest'):
        print(f"Chester.originalTest() returned: {chester.Chester.originalTest()}")
    
    if hasattr(chester.Chester, 'hello'):
        print(f"Chester.hello('Python') returned: {chester.Chester.hello('Python')}")
    
    # Check for error messages
    if hasattr(chester.Chester, 'error'):
        print(f"ERROR: {chester.Chester.error}")
    
except ImportError as e:
    print(f"Error importing Chester module: {e}")
except Exception as e:
    print(f"Error using Chester module: {e}")

print("\nTest completed.") 