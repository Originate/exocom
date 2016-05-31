require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
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
      ..post '/services', @_set-routing-config-controller
      ..post '/send/:message', @_send-controller
    @port = null


  close: ->
    | !@server  =>  return
    debug "no longer listening at port #{@port}"
    @server.close!


  listen: (+@port) ->
    | isNaN @port  =>  @emit 'error', 'Non-numerical port provided to ExoRelay#listen'
    @server = @app.listen @port
      ..on 'error', (err) ~>
        err = "port #{err.port} is already in use" if err.code is 'EADDRINUSE'
        @emit 'error', err
      ..on 'listening', ~> @emit 'listening', @port


  _config-controller: (req, res) ~>
    config = @exocom.get-config!
    res
      ..send config
      ..end!


  _set-routing-config-controller: (req, res) ~>
    switch (result = @exocom.set-routing-config req.body)
      | 'success'  =>  res.status(200).end!
      | _          =>  throw new Error "unknown error: #{result}"


  _send-controller: (req, res) ~>
    request-data = @_parse-request req
    @_log request-data
    switch (result = @exocom.send-message request-data)
      | 'success'             =>  res.status(200).end!
      | 'missing request id'  =>  res.status(400).end 'missing request id'
      | 'unknown message'     =>  res.status(404).end "unknown message: '#{request-data.message}'"
      | _                     =>  throw new Error "unknown result code: '#{@result}'"


  _log: ({name, id, response-to}) ->
    | response-to  =>  debug "received message '#{name}' with id '#{id}' in response to '#{response-to}'"
    | _            =>  debug "received message '#{name}' with id '#{id}'"


  # Returns the relevant data from a request
  _parse-request: (req) ->
    sender = req.body.sender
    name = req.params.message
    payload = req.body.payload
    response-to = req.body.response-to
    id = req.body.id
    {sender, name, response-to, payload, id}



module.exports = HttpListener
