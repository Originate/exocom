require! {
  './command-manager' : HandlerManager
  './command-sender' : CommandSender
  'rails-delegate' : delegate
  './http-listener' : HttpListener
}
debug = require('debug')('exorelay')


class ExoRelay

  ({exocomm-port} = {exocomm-port: 3010}) ->

    # manages the request handlers for incoming commands
    @command-handler = new HandlerManager!

    # sends outgoing commands to Exosphere
    @command-sender = new CommandSender {exocomm-port}

    # listens to incoming commands from Exosphere
    @http-listener = new HttpListener!
      ..on 'command', @_on-incoming-command

    delegate \close \listen from: @, to: @http-listener
    delegate \hasHandler \registerHandler \registerHandlers from: @, to: @command-handler


  send: (command, payload, reply-handler) ~>
    | reply-handler and typeof reply-handler isnt 'function'  =>  throw new Error 'The reply handler given to ExoRelay#send must be a function'

    request-id = @command-sender.send command, payload
    if reply-handler
      @command-handler.register-reply-handler request-id, reply-handler
    request-id


  _on-incoming-command: (request-data) ~>
    | !request-data.request-id  =>  return 'missing request id'
    @command-handler.handle-request request-data,
                                    reply: @command-sender.reply-method-for request-data.request-id
                                    send: @send



module.exports = ExoRelay
