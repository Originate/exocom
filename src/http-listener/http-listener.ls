require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
}
debug = require('debug')('exorelay:http-listener')


# The HTTP endpoint that listens for messages that services want to send
#
# Emits these events:
# - error: when it cannot bind to the given port
# - listening: when it listens at the given port
class HttpListener extends EventEmitter

  ->
    @app = express!
      ..use body-parser.json!
      ..get  '/status.json', @_status-controller
      ..get  '/config.json', @_config-controller
      ..post '/services', @_set-services-controller
      ..post '/send/:message', @_send-controller
    @port = null


  close: ->
    | !@server  =>  return
    debug "no longer listening at port #{@port}"
    @server.close!


  listen: (+@port) ->
    | isNaN @port =>  @emit 'error', 'Non-numerical port provided to ExoRelay#listen'
    @server = @app.listen @port
      ..on 'error', (err) ~>
        err = "port #{err.port} is already in use" if err.code is 'EADDRINUSE'
        @emit 'error', err
      ..on 'listening', ~> @emit 'listening', @port


  _config-controller: (req, res) ~>
    config = @listeners('get-config')[0]!
    res
      ..send config
      ..end!


  _set-services-controller: (req, res) ~>
    switch (result = @listeners('set-services')[0] req.body)
      | 'success'  =>  res.status(200).end!
      | _          =>  throw new Error "unknown error"


  _send-controller: (req, res) ~>
    request-data = @_parse-request req
    @_log request-data
    switch (result = @listeners('send-message')[0] request-data)
      | 'success'             =>  res.status(200).end!
      | 'missing request id'  =>  res.status(400).end 'missing request id'
      | 'unknown message'     =>  res.status(404).end "unknown message: '#{request-data.message}'"
      | _                     =>  throw new Error "unknown result code: '#{@result}'"


  # returns data about the current status of ExoComm
  _status-controller: (req, res) ~>
    @listeners('get-config')[0] (config) ->
      res.send JSON.stringify config


  _log: ({name, request-id, response-to}) ->
    | response-to  =>  debug "received message '#{name}' with id '#{request-id}' in response to '#{response-to}'"
    | _            =>  debug "received message '#{name}' with id '#{request-id}'"


  # Returns the relevant data from a request
  _parse-request: (req) ->
    name = req.params.message
    payload = req.body.payload
    response-to = req.body.response-to
    request-id = req.body.request-id
    {name, response-to, payload, request-id}



module.exports = HttpListener
