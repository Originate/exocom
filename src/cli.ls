require! {
  \chalk : {cyan, dim, green, red}
  'path'
  'require-yaml'
  './exo-service' : ExoService
}

exo-js-data = require path.join(__dirname, '..', 'package.json')
console.log dim "Exosphere Node.js service runner #{exo-js-data.version}\n"

service-data = require path.resolve('./service.yml')
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
