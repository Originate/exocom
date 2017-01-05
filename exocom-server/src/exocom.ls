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

  ({service-routes, @logger} = {}) ->

    @client-registry = new ClientRegistry {service-routes}

    @http-subsystem = new HttpSubsystem {exocom: @, @logger}
      ..on 'online', (port) ~> @emit 'http-online', port

    @message-cache = new MessageCache!

    @websocket = new WebSocketSubsystem {exocom: @, @logger}
      ..on 'online', (port) ~> @emit 'websockets-online', port


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


  # registers the given service instance that just came online
  register-client: (client) ~>
    @client-registry.register-client client


  # deregisters a service instance that went offline
  deregister-client: (client-name) ~>
    @client-registry.deregister-client client-name


  # sends the given message to all subscribers of it.
  send-message: (message-data) ~>
    # convert the outgoing message name from its internal version to the public version
    sender = @client-registry.clients[message-data.sender]
    public-message-name = @client-registry.outgoing-message-name message-data.name, sender
    message-data.original-name = message-data.name
    message-data.name = public-message-name
    message-data.timestamp = nanoseconds process.hrtime!
    # determine the subscribers
    subscribers = @client-registry.subscribers-for public-message-name
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
    @logger.messages messages: sent-messages, receivers: subscriber-names

    'success'



module.exports = ExoCom
