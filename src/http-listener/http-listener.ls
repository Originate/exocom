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


  listen: (@port) ->
    @app.listen port, ->
      debug "listening for Exosphere commands at port #{port}"


  on: (event-name, handler) ->
    | !event-name              =>  throw new Error 'no event name provided'
    | !handler                 =>  throw new Error 'no handler provided'
    | event-name is 'command'  =>  @handle-command = handler
    | _                        =>  throw new Error "unknown event: '#{event-name}'"


  _command-controller: (req, res) ~>
    [command, payload] = @_parse-request req
    debug "received command '#{command}'"
    if @handle-command command, payload
      res.status(200).end!
    else
      res.status(404).end!


  _overview-controller: (req, res) ->
    res.end!


  # Returns the relevant data from a request
  _parse-request: (req) ->
    command = req.params.command
    payload = req.body.payload
    [command, payload]



module.exports = HttpListener
