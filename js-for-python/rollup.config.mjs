import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import babel from '@rollup/plugin-babel';

export default {
  input: 'index.js',
  output: {
    file: 'dist/bundle.js',
    format: 'iife',
    name: 'Chester', // Global variable that will hold exports
    sourcemap: true,
  },
  plugins: [
    resolve({
      preferBuiltins: false,
    }),
    commonjs(),
    babel({
      babelHelpers: 'bundled',
      presets: [
        ['@babel/preset-env', { 
          targets: 'ie 11',  // IE 11 is compatible with ECMAScript 5.1
          modules: false,
          useBuiltIns: false, // Don't include any polyfills automatically
          corejs: false,      // Disable corejs polyfills
          include: [
            // Only include minimal transforms that js2py can handle
            '@babel/plugin-transform-arrow-functions',
            '@babel/plugin-transform-block-scoping'
          ],
          exclude: [
            // Exclude problematic transforms
            'transform-typeof-symbol',
            'transform-property-literals',
            'transform-member-expression-literals',
            'transform-reserved-words',
            'transform-spread',
            'transform-destructuring',
            'transform-object-rest-spread',
            'transform-modules-commonjs'
          ]
        }]
      ]
    }),
  ],
};
