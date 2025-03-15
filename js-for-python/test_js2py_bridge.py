#!/usr/bin/env python3
"""
Test script for js2py integration
"""

import sys
import os

# Add the path where our bridge file is
sys.path.append(os.path.join(os.path.dirname(__file__), "jvm/src/main/scala/chester"))

from Js2PyBridge import Js2PyBridge

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
