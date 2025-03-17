#!/usr/bin/env python3
"""
Test script for js2py integration
"""

import sys
import os

# Import our bridge module directly
from js2py_bridge import Js2PyBridge

def main():
    bridge = Js2PyBridge()
    
    # Test evaluating JavaScript code
    js_code = """
    var testVar = "Hello from JavaScript!";
    function testFunction(name) {
        return "Hello, " + name + "!";
    }
    """
    
    print("Evaluating JavaScript code...")
    success = bridge.eval_js(js_code)
    print(f"Evaluation {'succeeded' if success else 'failed'}")
    
    # Test getting a variable
    print("\nGetting variable 'testVar'...")
    result = bridge.get_value("testVar")
    print(f"Value: {result}")
    
    # Test calling a function
    print("\nCalling function 'testFunction' with argument 'World'...")
    result = bridge.call_function("testFunction", "World")
    print(f"Result: {result}")
    
if __name__ == "__main__":
    main()
