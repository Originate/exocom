require! {
  \chalk : {cyan, dim, green, red}
  './exo-service' : ExoService
}


bootstrap = (handlers) ->
  new ExoService {
    exocom-host: process.env.EXOCOM_HOST ? "localhost"
    exocom-port: process.env.EXOCOM_PORT ? 80
    handlers
    role: process.env.ROLE
  }
    ..on 'online', (port) ->
      console.log dim "Ctrl-C to stop"
      console.log "online at port #{cyan port}"
    ..on 'error', (err) -> console.log red err
    ..on 'offline', -> console.log red 'SERVER CLOSED'
    ..connect!


module.exports = bootstrap
