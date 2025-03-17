#!/usr/bin/env python3
"""
js2py_build: A build script to translate JavaScript to Python using js2py.translate_file
"""

import os
import sys
import js2py
import argparse

# Path to the bundled JavaScript file
BUNDLE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(__file__)),
    'dist', 'bundle.js'
)

# Path for the translated Python module
TRANSLATED_MODULE_PATH = os.path.join(
    os.path.dirname(__file__),
    'generated_js_module.py'
)

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
    
    # Translate the JavaScript to Python
    try:
        print(f"Translating JavaScript bundle to Python module...")
        js2py.translate_file(BUNDLE_PATH, TRANSLATED_MODULE_PATH)
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
