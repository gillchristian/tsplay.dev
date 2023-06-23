import replace from '@rollup/plugin-replace'
import resolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import babel from 'rollup-plugin-babel'
import postcss from 'rollup-plugin-postcss'
import image from '@rollup/plugin-image'
import execute from 'rollup-plugin-execute'
import progress from 'rollup-plugin-progress'
import serve from 'rollup-plugin-serve'
import {eslint} from 'rollup-plugin-eslint'
import {terser} from 'rollup-plugin-terser'
import externalGlobals from 'rollup-plugin-external-globals'
import ignore from 'rollup-plugin-ignore'
import analyze from 'rollup-plugin-analyzer'
import filesize from 'rollup-plugin-filesize'

const isProd = process.env.NODE_ENV === 'production'
// const isWatch = process.env.ROLLUP_WATCH;
const extensions = ['.js', '.ts', '.tsx']

export default {
  input: 'src/index.tsx',
  external: ['typescript', 'react'],
  treeshake: true,
  output: {
    file: 'dist/index.js',
    format: 'amd',
  },
  plugins: [
    isProd && filesize(),
    isProd &&
      analyze({
        summaryOnly: true,
      }),

    progress(),
    execute('node scripts/open-playground'),
    image(),
    postcss({minimize: true}),
    replace({
      'process.env.NODE_ENV': JSON.stringify(
        isProd ? 'production' : 'development',
      ),
    }),
    eslint({throwOnError: true}),
    resolve({
      extensions,
      modulesOnly: false,
    }),
    commonjs({
      include: /node_modules/,
      sourceMap: false,
    }),
    babel({
      extensions,
      exclude: /node_modules/,
      babelrc: false,
      runtimeHelpers: true,
      presets: [
        '@babel/preset-env',
        '@babel/preset-react',
        '@babel/preset-typescript',
      ],
    }),
    ignore(['react']),
    externalGlobals({react: 'window.react'}),
    isProd && terser(),
    !isProd &&
      serve({
        contentBase: 'dist',
        port: 5000,
      }),
  ],
}
