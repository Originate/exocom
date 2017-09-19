var webpackNodeExternals = require('webpack-node-externals')
var path = require('path')

module.exports = {
  entry: './src/index.js',
  target: 'node',
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'bundle.js',
    libraryTarget: 'commonjs2',
  },
  externals: [webpackNodeExternals()],
}
