require! {
  './command-manager' : HandlerManager
  './command-sender' : CommandSender
  './http-listener' : HttpListener
}
debug = require('debug')('exorelay')


class ExoRelay

  ({exo-messaging-port} = {}) ->

    # manages the request handlers for incoming commands
    @command-handler = new HandlerManager!

    # sends outgoing commands to Exosphere
    @command-sender = new CommandSender {exo-messaging-port}

    # listens to incoming commands from Exosphere
    @http-listener = new HttpListener!
      ..on 'command', @_on-incoming-command


  close: ->
    @http-listener.close!


  has-handler: (command) ->
    @command-handler.has-handler command


  listen: (port, done) ->
    @http-listener.listen port, done


  register-handler: (command, handler) ->
    @command-handler.register-handler command, handler


  register-handlers: (handlers) ->
    @command-handler.register-handlers handlers


  send: (command, reply-handler) ->
    request-id = @command-sender.send command
    if reply-handler
      @command-handler.register-reply-handler request-id, reply-handler
    request-id


  _on-incoming-command: (request-data) ~>
    @command-handler.handle-request request-data



module.exports = ExoRelay
