// Import Map polyfill from es6-map-shim
import Map from 'es6-map-shim';

// Make Map available globally for scripts
if (typeof Map === 'undefined') {
  window.Map = Map;
}

// Import the JavaScript generated from Scala.js (using fastLinkJS for better debugging)
import * as chesterJS from './js/target/scala-3.6.4/js-for-python-fastopt/main.mjs';

// Create a simplified global object for Python to access
// Only use ES5.1 compatible features
var ChesterCompat = {};

// Directly expose Scala.js exported functions
// Direct assignment of the test function
ChesterCompat.test = chesterJS.test;

// Direct assignment of the helloFromJs constant
ChesterCompat.helloFromJs = chesterJS.helloFromJs;

// Validate that required exports are defined
if (typeof ChesterCompat.test !== 'function') {
  throw new Error("Required function 'test' is not defined in Scala.js output");
}

if (typeof ChesterCompat.helloFromJs === 'undefined') {
  throw new Error("Required constant 'helloFromJs' is not defined in Scala.js output");
}

// Set global variable for js2py to access more easily
window.Chester = ChesterCompat;
