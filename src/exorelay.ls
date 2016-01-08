require! {
  './command-manager' : HandlerManager
  './http-listener' : HttpListener
  'request'
}
debug = require('debug')('exorelay')


class ExoRelay

  ({@exo-messaging-port}) ->

    # manages the request handlers
    @command-handler = new HandlerManager!

    # listens to incoming commands
    @http-listener = new HttpListener!
      ..on 'command', @_on-incoming-command


  close: ->
    @http-listener.close!


  listen: (port, done) ->
    @http-listener.listen port, done


  register-handler: (command, handler) ->
    @command-handler.register-handler command, handler


  send: (command, done) ->
    debug "sending command '#{command}'"
    options =
      method: 'POST'
      url: "http://localhost:#{@exo-messaging-port}/send/#{command}"
    request options, (err, @exo-messaging-response, body) ~>
      done!


  _on-incoming-command: (command, payload) ~>
    @command-handler.handle-request command, payload



module.exports = ExoRelay
