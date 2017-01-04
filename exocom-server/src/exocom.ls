require! {
  './client-registry/client-registry' : ClientRegistry
  'events' : {EventEmitter}
  './http-subsystem/http-subsystem' : HttpSubsystem
  './message-cache/message-cache' : MessageCache
  'nanoseconds'
  'process'
  'rails-delegate' : {delegate, delegate-event}
  './websocket-subsystem/websocket-subsystem' : WebSocketSubsystem
}
debug = require('debug')('exocom')


class ExoCom extends EventEmitter

  ({@service-messages} = {}) ->

    @client-registry = new ClientRegistry {@service-messages}

    @http-subsystem = new HttpSubsystem @
      ..on 'online', (port) ~> @emit 'http-online', port

    @message-cache = new MessageCache!

    @websocket = new WebSocketSubsystem @
      ..on 'online', (port) ~> @emit 'websockets-online', port

    delegate-event 'error', 'warn' from: @websocket, to: @
    delegate-event 'error' from: @http-subsystem, to: @


  # returns the current configuration of this ExoCom instance
  get-config: ~>
    {
      clients: @client-registry.clients
      routes: @client-registry.routes
    }


  close: ->
    @http-subsystem.close!
    @websocket.close!


  # bind to the given port to send socket messages
  listen: (port) ->
    express-server = @http-subsystem.listen port
    @websocket.listen port, express-server
    debug "Listening at port #{port}"


  # registers the service with the given data
  # as a sender and receiver of messages
  register-client: (routing-config) ~>
    @client-registry.register-client routing-config


  # deregisters a service with the given data
  # as a sender and receiver of messages
  deregister-client: (service-name) ~>
    @client-registry.deregister-client service-name


  # sends the given message to all subscribers of it.
  send-message: (message-data) ~>
    # convert the outgoing message name from its internal version to the public version
    sender = @client-registry.clients[message-data.sender]
    external-message-name = @client-registry.outgoing-message-name message-data.name, sender
    message-data.original-name = message-data.name
    message-data.name = external-message-name
    message-data.timestamp = nanoseconds process.hrtime!
    # determine the subscribers
    subscribers = @client-registry.subscribers-for external-message-name
    return 'no receivers' unless subscribers
    subscriber-names = [subscriber.name for subscriber in subscribers]

    # calculate a message's response time if it is a reply
    if message-data.response-to
      original-timestamp  = @message-cache.get-original-timestamp message-data.id
      message-data.response-time = message-data.timestamp - original-timestamp
    else
      @message-cache.push message-data.id, message-data.timestamp

    # send the message to the subscribers
    debug "sending '#{message-data.name}' to #{subscriber-names}"
    sent-messages = @websocket.send-message-to-services message-data, subscribers
    @emit 'message', messages: sent-messages, receivers: subscriber-names

    'success'



module.exports = ExoCom
