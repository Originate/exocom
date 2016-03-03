require! {
  \chalk : {cyan, dim, green, red}
  '../package.json' : {name, version}
  'path'
  './exo-service' : ExoService
}

console.log dim "Exosphere Node.js service runner #{version}\n"

service-data = require path.resolve('./package.json')
console.log "Running #{green service-data.name} #{green service-data.version}\n"


new ExoService parse-options!
  ..on 'online', (port) ->
    console.log dim "Ctrl-C to stop"
    console.log "online at port #{cyan port}"
  ..on 'error', (err) -> console.log red err
  ..on 'offline', -> console.log red 'SERVER CLOSED'
  ..listen!


function parse-options
  exocom-port: process.env.EXOCOM_PORT
  exorelay-port: process.env.EXORELAY_PORT
  service-name: process.env.SERVICE_NAME
