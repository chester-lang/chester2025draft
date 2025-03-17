import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';
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
          useBuiltIns: 'usage',
          corejs: 3,
          include: [
            '@babel/plugin-transform-arrow-functions',
            '@babel/plugin-transform-block-scoping',
            '@babel/plugin-transform-classes',
            '@babel/plugin-transform-for-of'
          ],
          exclude: ['transform-typeof-symbol'] // Avoid issues with js2py
        }]
      ]
    }),
    terser({
      compress: {
        dead_code: true,
      },
    }),
  ],
};
