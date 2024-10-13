import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import terser from '@rollup/plugin-terser';
import babel from '@rollup/plugin-babel';

export default {
  input: 'index.js', // Adjust the input path as needed
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
        ['@babel/preset-env', { targets: {rhino: "1.7.15"} }]
      ]
    }),
// broken on termux
/*    terser({
      compress: {
        dead_code: true, // Enable dead code removal
      },
    }),*/
  ],
};
