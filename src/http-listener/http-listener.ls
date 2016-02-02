require! {
  'body-parser'
  'events' : {EventEmitter}
  'express'
}
debug = require('debug')('exorelay:http-listener')


# The HTTP endpoint into which the Exosphere environment can
# POST new commands.
class HttpListener extends EventEmitter

  ->
    @app = express!
      ..use body-parser.json!
      ..get '/run', @_overview-controller
      ..post '/run/:command', @_command-controller


  close: ->
    if @server
      debug "no longer listening at port #{@port}"
      @server.close!


  listen: (+@port, done) ->
    | typeof port is 'function'  =>  return @listen 4000, port
    | isNaN @port                =>  return @emit 'error', Error 'Non-numerical port provided to ExoRelay#listen'

    @server = @app.listen port, ->
      debug "listening for Exosphere commands at port #{port}"
      done?!


  _command-controller: (req, res) ~>
    request-data = @_parse-request req
    @_log request-data
    switch (result = @listeners('command')[0] request-data)
      | 'success'             =>  res.status(200).end!
      | 'missing request id'  =>  res.status(400).end 'missing request id'
      | 'unknown command'     =>  res.status(404).end "unknown command: '#{request-data.command}'"
      | _                     =>  return @emit 'error', Error "unknown result code: '#{@result}'"


  _log: ({command, request-id, response-to}) ->
    | response-to  =>  debug "received command '#{command}' with id '#{request-id}' in response to '#{response-to}'"
    | _            =>  debug "received command '#{command}' with id '#{request-id}'"


  _overview-controller: (req, res) ->
    res.end!


  # Returns the relevant data from a request
  _parse-request: (req) ->
    command = req.params.command
    payload = req.body.payload
    response-to = req.body.response-to
    request-id = req.body.request-id
    {command, response-to, payload, request-id}



module.exports = HttpListener
