require!{
  'path'
  'fs'
}

node-modules = {}
apps = {}

fs.readdir-sync('node_modules')
  .filter((x) -> x isnt '.bin')
  .for-each((mod) -> node-modules[mod] = 'commonjs ' + mod)

module.exports = { 
  entry: ['./src/exo-service']
  target: 'node'
  node:
    __filename: yes
    __dirname: yes
  output:
    path: path.join process.cwd!, 'dist'
    filename: 'exo-js.js'
    libraryTarget: 'commonjs2'
    library: 'ExoService'
  externals: node-modules
  module:
    loaders:
      * test: /\.ls$/
        loader: \livescript
        exclude: /node_modules/
      * test: /\.json$/
        loader: \json
  resolve:
    extensions: ["", ".js", ".json", ".ls"]
}
