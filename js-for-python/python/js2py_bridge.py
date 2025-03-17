#!/usr/bin/env python3
"""
js2py_bridge: A bridge between JavaScript and Python using js2py.
"""

import os
import sys
import js2py
import json

class Js2PyBridge:
    """A bridge for executing JavaScript in Python using js2py."""
    
    def __init__(self):
        """Initialize the Js2PyBridge with the bundled JavaScript."""
        self.context = js2py.EvalJs()
        # Path to the bundled JavaScript file
        bundle_path = os.path.join(
            os.path.dirname(os.path.dirname(__file__)),
            'dist', 'bundle.js'
        )
        
        # Check if bundle exists
        if not os.path.exists(bundle_path):
            raise FileNotFoundError(f"Bundle file not found at {bundle_path}. Run 'pnpm run build' first.")
        
        # Load the bundled JavaScript
        with open(bundle_path, 'r') as f:
            bundle_js = f.read()
        
        # Execute the bundled JavaScript in the context
        try:
            self.context.execute(bundle_js)
            print("JavaScript bundle loaded successfully.")
        except Exception as e:
            print(f"Error loading JavaScript bundle: {e}")
            raise
    
    def eval_js(self, js_code):
        """Evaluate JavaScript code in the context."""
        try:
            self.context.execute(js_code)
            return True
        except Exception as e:
            print(f"Error evaluating JavaScript: {e}")
            return False
    
    def get_value(self, var_name):
        """Get a JavaScript variable value."""
        try:
            return self.context.eval(var_name)
        except Exception as e:
            print(f"Error getting variable '{var_name}': {e}")
            return None
    
    def call_function(self, func_name, *args):
        """Call a JavaScript function with arguments."""
        try:
            # Convert the arguments to JSON strings to ensure compatibility
            args_json = [json.dumps(arg) if isinstance(arg, (dict, list)) else arg for arg in args]
            args_str = ', '.join([f"'{arg}'" if isinstance(arg, str) else str(arg) for arg in args_json])
            
            # Call the function
            js_code = f"{func_name}({args_str})"
            return self.context.eval(js_code)
        except Exception as e:
            print(f"Error calling function '{func_name}': {e}")
            return None
