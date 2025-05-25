// Import from the project scala version
// Current project Scala version is defined in build.sbt as scala3Version
const SCALA_VERSION = '3.7.1-RC2'; // Update this when the project's Scala version changes

// Import the compiled Scala.js modules
import * as Chester from './js/target/scala-3.7.1-RC2/js-for-lua-fastopt/main.mjs';

// Re-export the modules for bundling
export default Chester; 