#!/usr/bin/env python3
"""
Test script for js2py integration using the generated module
"""

import os
import sys

# Ensure the generated module exists first
if not os.path.exists(os.path.join(os.path.dirname(__file__), 'chester.py')):
    print("Error: The chester.py module doesn't exist.")
    print("Run 'python js2py_build.py' first to generate it.")
    sys.exit(1)

# Import the chester module
import chester

def main():
    # Display available functions and objects in the chester module
    print("\nAvailable JavaScript objects and functions in the chester module:")
    js_items = [item for item in dir(chester) 
               if not item.startswith('__') and not item.startswith('_')]
    for item in js_items:
        print(f"  - {item}")
    
    print("\nTrying to access JavaScript functionality:")
    
    # Try to call any exposed functions or access values from the JavaScript
    # Note: The actual method names will depend on how the JavaScript was written
    # and what functions were exported
    try:
        # Example - modify these based on your actual JavaScript exports
        if hasattr(chester, 'test'):
            result = chester.test()
            print(f"Result from test(): {result}")
        
        # You might need to explore the module structure further
        # depending on how your JavaScript is organized
    except Exception as e:
        print(f"Error accessing JavaScript: {e}")
    
if __name__ == "__main__":
    main()
