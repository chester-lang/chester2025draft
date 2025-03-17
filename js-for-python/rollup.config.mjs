import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';
import babel from '@rollup/plugin-babel';

export default {
  input: 'index.js',
  output: {
    file: 'dist/bundle.js',
    format: 'cjs',
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
          targets: { 
            browsers: ['ie 11'], // Targets ES5.1 compatibility
          },
          modules: false,
          useBuiltIns: 'usage',
          corejs: 3,
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
