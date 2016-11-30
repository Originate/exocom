require! {
  \chalk : {cyan, dim, green, red}
  'path'
  'require-yaml'
  './exo-service' : ExoService
}

exo-js-data = require path.join(__dirname, '..', 'package.json')
console.log dim "Exosphere Node.js service runner #{exo-js-data.version}\n"

service-data = require path.resolve('./service.yml')
console.log "Running #{green service-data.name}\n"


new ExoService parse-options!
  ..on 'online', (port) ->
    console.log dim "Ctrl-C to stop"
    console.log "online at port #{cyan port}"
  ..on 'error', (err) -> console.log red err
  ..on 'offline', -> console.log red 'SERVER CLOSED'
  ..connect!


function parse-options
  exocom-host: process.env.EXOCOM_HOST ? "localhost"
  exocom-port: process.env.EXOCOM_PORT
  service-name: process.env.SERVICE_NAME
  internal-namespace: service-data.namespace
