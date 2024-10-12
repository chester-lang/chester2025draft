import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';

export default {
  input: 'index.js', // Adjust the input path as needed
  output: {
    file: 'dist/bundle.js',
    format: 'es', // Output as an ES6 module
    sourcemap: true,
  },
  plugins: [
    resolve({
      preferBuiltins: false,
    }),
    commonjs(),
// broken on termux
/*    terser({
      compress: {
        dead_code: true, // Enable dead code removal
      },
    }),*/
  ],
};
