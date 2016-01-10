require! {
  'body-parser'
  'express'
}
debug = require('debug')('exorelay:http-listener')


# The HTTP endpoint into which the Exosphere environment can
# POST new commands.
class HttpListener

  ->
    @app = express!
      ..use body-parser.json!
      ..get '/run', @_overview-controller
      ..post '/run/:command', @_command-controller


  close: ->
    debug "no longer listening at port #{@port}"
    @server.close!


  listen: (@port, done) ->
    | typeof port is 'function'  =>  return @listen 4000, port

    @server = @app.listen port, ->
      debug "listening for Exosphere commands at port #{port}"
      done?!


  on: (event-name, handler) ->
    | !event-name              =>  throw new Error 'no event name provided'
    | !handler                 =>  throw new Error 'no handler provided'
    | event-name is 'command'  =>  @handle-command = handler
    | _                        =>  throw new Error "unknown event: '#{event-name}'"


  _command-controller: (req, res) ~>
    [command, replying-to, payload] = @_parse-request req
    @_log command, replying-to
    if @handle-command {command, replying-to, payload}
      res.status(200).end!
    else
      res.status(404).end!


  _log: (command, replying-to) ->
    | replying-to  =>  debug "received command '#{command}' in reply to '#{replying-to}'"
    | _            =>  debug "received command '#{command}'"


  _overview-controller: (req, res) ->
    res.end!


  # Returns the relevant data from a request
  _parse-request: (req) ->
    command = req.params.command
    payload = req.body.payload
    replying-to = req.body['replying-to']
    [command, replying-to, payload]



module.exports = HttpListener
