// Import WeakMap from core-js directly
import WeakMapPolyfill from 'core-js/es/weak-map';

// Make WeakMap available globally
if (typeof WeakMap === 'undefined') {
  var WeakMap = WeakMapPolyfill;
}

// Import the JavaScript generated from Scala.js (using fastLinkJS for better debugging)
import * as chesterJS from './js/target/scala-3.6.4/js-for-python-fastopt/main.mjs';

// Create a simplified global object for Python to access
// Only use ES5.1 compatible features
var ChesterCompat = {};

// Add wrapper functions that expose only what Python needs
// This isolates Python from unsupported JS features

// Test function
ChesterCompat.test = function() {
  return "Chester JavaScript compatibility layer is working!";
};

// Add any needed functions from the original Chester JS
// with simplified interfaces if necessary
try {
  if (typeof chesterJS.test === 'function') {
    ChesterCompat.originalTest = function() {
      try {
        return chesterJS.test();
      } catch(e) {
        return "Error calling original test function: " + e.message;
      }
    };
  }
  
  if (typeof chesterJS.helloFromJs === 'function') {
    ChesterCompat.hello = function(name) {
      try {
        return chesterJS.helloFromJs(name || "World");
      } catch(e) {
        return "Error in hello function: " + e.message;
      }
    };
  }
  
  // Add more wrapper functions as needed
  // ...
  
} catch(e) {
  ChesterCompat.error = "Error initializing compatibility layer: " + e.message;
}

// Export the compatibility layer
export default ChesterCompat;

// Also export individual functions for convenience
export const test = ChesterCompat.test;
export const hello = ChesterCompat.hello;
