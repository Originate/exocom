require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
}
debug = require('debug')('exorelay:http-listener')


# The HTTP endpoint into which the Exosphere environment can
# POST new messages.
#
# Emits these events:
# - online
# - offline
# - error
class HttpListener extends EventEmitter

  ->
    @app = express!
      ..use body-parser.json!
      ..get '/status', @_status-controller
      ..post '/run/:message', @_message-controller
    @port = null


  close: ->
    return unless @server
    @server.close!
    @server = null
    @port = null
    debug "no longer listening at port #{@port}"
    @emit 'offline'


  listen: (@port) ->
    | isNaN @port  =>  return @emit 'error', Error 'Non-numerical port provided to ExoRelay#listen'

    @server = @app.listen @port, ~>
      debug "listening for Exosphere messages at port #{port}"
      @emit 'online', @port


  _message-controller: (req, res) ~>
    request-data = @_parse-request req
    @_log request-data
    switch (result = @listeners('message')[0] request-data)
      | 'success'             =>  res.status(200).end!
      | 'missing request id'  =>  res.status(400).end 'missing request id'
      | 'unknown message'     =>  res.status(404).end "unknown message: '#{request-data.message}'"
      | _                     =>  return @emit 'error', Error "unknown result code: '#{@result}'"


  _log: ({message, request-id, response-to}) ->
    | response-to  =>  debug "received message '#{message}' with id '#{request-id}' in response to '#{response-to}'"
    | _            =>  debug "received message '#{message}' with id '#{request-id}'"


  _status-controller: (req, res) ->
    res.end!


  # Returns the relevant data from a request
  _parse-request: (req) ->
    message = req.params.message
    payload = req.body.payload
    response-to = req.body.response-to
    request-id = req.body.request-id
    {message, response-to, payload, request-id}



module.exports = HttpListener
