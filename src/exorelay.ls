require! {
  './command-manager' : HandlerManager
  './command-sender' : CommandSender
  'rails-delegate' : delegate
  './http-listener' : HttpListener
}
debug = require('debug')('exorelay')


class ExoRelay

  ({exocomm-port} = {}) ->

    # manages the request handlers for incoming commands
    @command-handler = new HandlerManager!

    # sends outgoing commands to Exosphere
    @command-sender = new CommandSender {exocomm-port}

    # listens to incoming commands from Exosphere
    @http-listener = new HttpListener!
      ..on 'command', @_on-incoming-command

    delegate \close \listen from: @, to: @http-listener
    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @command-handler


  send: (command, reply-handler) ->
    request-id = @command-sender.send command
    if reply-handler
      @command-handler.register-reply-handler request-id, reply-handler
    request-id


  _on-incoming-command: (request-data) ~>
    @command-handler.handle-request request-data



module.exports = ExoRelay
