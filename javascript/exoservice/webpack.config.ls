require!{
  'path'
  'fs'
  'webpack-node-externals'
}

module.exports = {
  entry:
    'bundle': './src/index.ls'
  target: 'node'
  output:
    path: path.join __dirname, 'dist'
    filename: '[name].js'
    libraryTarget: 'commonjs2'
    library: 'ExoService'
  externals: [webpackNodeExternals()]
  module:
    rules:
      * test: /\.ls$/
        use: <[ livescript-loader ]>
        exclude: /node_modules/
      ...
  resolve:
    extensions: [".js", ".json", ".ls"]
}
