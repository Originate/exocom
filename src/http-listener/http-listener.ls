require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
}
debug = require('debug')('exorelay:http-listener')


# The HTTP endpoint that listens for commands that services want to send
#
# Emits these events:
# - error: when it cannot bind to the given port
# - listening: when it listens at the given port
class HttpListener extends EventEmitter

  ->
    @app = express!
      ..use body-parser.json!
      ..get  '/status.json', @_status-controller
      ..post '/register-service', @_register-service-controller
      ..post '/send/:command', @_send-controller
    @port = null


  close: ->
    | !@server  =>  return
    debug "no longer listening at port #{@port}"
    @server.close!


  listen: (+@port) ->
    | isNaN @port =>  emit 'error', 'Non-numerical port provided to ExoRelay#listen'
    @server = @app.listen @port
      ..on 'error', (err) ~>
        err = "port #{err.port} is already in use" if err.code is 'EADDRINUSE'
        @emit 'error', err
      ..on 'listening', ~> @emit 'listening', @port


  _register-service-controller: (req, res) ~>
    request-data = req.body.payload
    debug "service '#{request-data.name}' requesting to register"
    switch (result = @listeners('register-service')[0] request-data)
      | 'success'  =>  res.status(200).end!


  _send-controller: (req, res) ~>
    request-data = @_parse-request req
    @_log request-data
    switch (result = @handle-command request-data)
      | 'success'             =>  res.status(200).end!
      | 'missing request id'  =>  res.status(400).end 'missing request id'
      | 'unknown command'     =>  res.status(404).end "unknown command: '#{request-data.command}'"
      | _                     =>  throw new Error "unknown result code: '#{@result}'"


  # returns data about the current status of ExoComm
  _status-controller: (req, res) ~>
    @listeners('get-config')[0] (config) ->
      res.send JSON.stringify config


  _log: ({command, request-id, response-to}) ->
    | response-to  =>  debug "received command '#{command}' with id '#{request-id}' in response to '#{response-to}'"
    | _            =>  debug "received command '#{command}' with id '#{request-id}'"


  # Returns the relevant data from a request
  _parse-request: (req) ->
    command = req.params.command
    payload = req.body.payload
    response-to = req.body.response-to
    request-id = req.body.request-id
    {command, response-to, payload, request-id}



module.exports = HttpListener
