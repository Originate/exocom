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


  listen: (port, done) ->
    @http-listener.listen port, done


  register-handler: (command, handler) ->
    @command-handler.register-handler command, handler


  send: (command, done) ->
    @command-sender.send command, done


  _on-incoming-command: (command, payload) ~>
    @command-handler.handle-request command, payload



module.exports = ExoRelay
