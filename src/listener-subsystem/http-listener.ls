require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
  'http'
}
debug = require('debug')('exocom:http-listener')


# The HTTP endpoint that listens for messages that services want to send
#
# Emits these events:
# - error: when it cannot bind to the given port
# - listening: when it listens at the given port
class HttpListener extends EventEmitter

  # This method receives an object that provides access to Exocom's API.
  # It is used to call into other Exocom services.
  (@exocom) ->
    @app = express!
      ..use body-parser.json!
      ..get  '/config.json', @_config-controller
    @port = null


  close: ->
    | !@server  =>  return
    debug "HTTP no longer listening at port #{@port}"
    @server.close!


  listen: (+@port) ->
    | isNaN @port  =>  @emit 'error', 'Non-numerical port provided to ExoCom#listen'
    @server = http.create-server @app
      ..listen @port
      ..on 'error', (err) ~>
        err = "port #{err.port} is already in use" if err.code is 'EADDRINUSE'
        @emit 'error', err
      ..on 'listening', ~> @emit 'http-bound', @port


  _config-controller: (req, res) ~>
    res
      ..send @exocom.get-config!
      ..end!


  _log: ({name, id, response-to}) ->
    | response-to  =>  debug "received message '#{name}' with id '#{id}' in response to '#{response-to}'"
    | _            =>  debug "received message '#{name}' with id '#{id}'"



module.exports = HttpListener
