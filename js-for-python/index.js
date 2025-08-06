// Import Map polyfill from es6-map-shim
import Map from 'es6-map-shim';

// Make Map available globally for scripts
if (typeof Map === 'undefined') {
  window.Map = Map;
}

// Import the JavaScript generated from Scala.js (using fastLinkJS for better debugging)
import * as chesterJS from './js/target/scala-3.7.3-RC1/js-for-python-fastopt/main.mjs';

// Set global variable for js2py to access the module directly
window.Chester = chesterJS;

// Validate that required exports are defined
if (typeof chesterJS.test !== 'function') {
  throw new Error("Required function 'test' is not defined in Scala.js output");
}

if (typeof chesterJS.helloFromJs === 'undefined') {
  throw new Error("Required constant 'helloFromJs' is not defined in Scala.js output");
}
