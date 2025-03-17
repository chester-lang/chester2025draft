#!/usr/bin/env python3
"""
js2py_build: A build script to translate JavaScript to Python using js2py.translate_file
"""

import os
import sys
import js2py
import argparse
import re

# Path to the bundled JavaScript file
BUNDLE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(__file__)),
    'dist', 'bundle.js'
)

# Path for the translated Python module
TRANSLATED_MODULE_PATH = os.path.join(
    os.path.dirname(__file__),
    'chester.py'
)

# Path for the modified bundle (with WeakMap replaced)
MODIFIED_BUNDLE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(__file__)),
    'dist', 'bundle.modified.js'
)

# Python template to append to the generated module
PYTHON_TEMPLATE = """
# Expose top-level functions and variables for a more Pythonic API
test = chester.Chester.test
helloFromJs = chester.Chester.helloFromJs

# More top-level exports can be added here
"""

def preprocess_js_bundle():
    """
    Preprocess the JavaScript bundle to replace problematic constructs
    before running js2py
    """
    # Read the original bundle
    with open(BUNDLE_PATH, 'r') as f:
        js_code = f.read()
    
    # Replace WeakMap with Map
    js_code = js_code.replace('new WeakMap()', 'new Map()')
    
    # Write the modified bundle
    with open(MODIFIED_BUNDLE_PATH, 'w') as f:
        f.write(js_code)
    
    return MODIFIED_BUNDLE_PATH

def post_process_py_module():
    """
    Add Python-friendly shortcuts to the translated module
    """
    if not os.path.exists(TRANSLATED_MODULE_PATH):
        return False
    
    with open(TRANSLATED_MODULE_PATH, 'r') as f:
        py_code = f.read()
    
    # Append our Python template to the end of the file
    py_code += PYTHON_TEMPLATE
    
    with open(TRANSLATED_MODULE_PATH, 'w') as f:
        f.write(py_code)
    
    return True

def translate_js_to_py(force=False):
    """
    Translate the JavaScript bundle to a Python module using js2py.translate_file
    
    Args:
        force (bool): If True, force retranslation even if the module exists
    """
    # Check if bundle exists
    if not os.path.exists(BUNDLE_PATH):
        print(f"Error: Bundle file not found at {BUNDLE_PATH}")
        print("Please run 'pnpm run build' first to generate the JavaScript bundle.")
        return False
    
    # Check if translation is needed
    if os.path.exists(TRANSLATED_MODULE_PATH) and not force:
        bundle_mtime = os.path.getmtime(BUNDLE_PATH)
        module_mtime = os.path.getmtime(TRANSLATED_MODULE_PATH)
        
        if module_mtime > bundle_mtime:
            print(f"Python module is up to date ({TRANSLATED_MODULE_PATH})")
            return True
    
    # Preprocess the JavaScript bundle
    print(f"Preprocessing JavaScript bundle...")
    modified_bundle_path = preprocess_js_bundle()
    
    # Translate the JavaScript to Python
    try:
        print(f"Translating JavaScript bundle to Python module...")
        js2py.translate_file(modified_bundle_path, TRANSLATED_MODULE_PATH)
        
        # Post-process to add Python-friendly shortcuts
        print(f"Adding Python-friendly shortcuts...")
        post_process_py_module()
        
        print(f"Successfully translated to {TRANSLATED_MODULE_PATH}")
        return True
    except Exception as e:
        print(f"Error translating JavaScript bundle: {e}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Translate JavaScript bundle to Python module')
    parser.add_argument('--force', '-f', action='store_true', help='Force retranslation even if module exists')
    args = parser.parse_args()
    
    success = translate_js_to_py(force=args.force)
    return 0 if success else 1

if __name__ == "__main__":
    sys.exit(main())
