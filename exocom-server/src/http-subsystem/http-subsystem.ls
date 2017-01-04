require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
  'http'
}
debug = require('debug')('exocom:http-subsystem')


# The administration endpoint for Exocom
#
# Emits these events:
# - error: when it cannot bind to the given port
# - online: when it listens at the given port
class HttpSubsystem extends EventEmitter

  # param @exocom: for making calls into the core
  (@exocom) ->

    # the Express instance that implements the HTTP interface
    @app = express!
      ..use body-parser.json!
      ..get  '/config.json', @_on-config-request

    # the port at which the HTTP server listens
    @port = null

    # whether this subsystem is online
    @online = no


  close: ->
    | !@server  =>  return
    debug "HTTP subsystem closing"
    @server.close!
    @online = no


  listen: (+@port) ->
    | isNaN @port  =>  @emit 'error', 'Non-numerical port provided to ExoCom#listen'
    @server = http.create-server @app
      ..listen @port
      ..on 'error', (err) ~>
        | err.code is 'EADDRINUSE'  =>  @emit 'error', "port #{err.port} is already in use"
        | otherwise                 =>  @emit 'error', err
      ..on 'listening', ~>
        @online = yes
        @emit 'online', @port



  _on-config-request: (req, res) ~>
    res
      ..send @exocom.get-config!
      ..end!



module.exports = HttpSubsystem
