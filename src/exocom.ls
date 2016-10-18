require! {
  './client-registry/client-registry' : ClientRegistry
  'events' : {EventEmitter}
  './listener-subsystem/listener-subsystem' : ListenerSubsystem
  './message-cache/message-cache' : MessageCache
  './message-sender/message-sender' : MessageSender
  'nanoseconds'
  'process'
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exocom')


class ExoCom extends EventEmitter

  ->
    @client-registry    = new ClientRegistry
    @listener-subsystem = new ListenerSubsystem @
    @message-cache = new MessageCache!
    @message-sender = new MessageSender @

    delegate 'close' 'listen' 'zmqPort' 'httpPort', from: @, to: @listener-subsystem
    delegate 'clearPorts', from: @, to: @message-sender
    delegate-event 'zmq-bound' 'http-bound' 'error', from: @listener-subsystem, to: @


  # returns the current configuration of this ExoCom instance
  get-config: ~>
    {
      services: @client-registry.clients
      routes: @client-registry.routes
    }


  # bind to the given port to send socket messages
  listen: (ports) ->
    @listener-subsystem.listen ports
    debug "ZMQ bound at port #{ports.zmq-port}, HTTP listening at port #{ports.http-port}"


  # registers the services with the given data
  # as a sender and receiver of messages
  set-routing-config: (routing-config) ~>
    debug 'receiving service setup'
    @client-registry.set-routing-config routing-config
    @message-sender.bind-services @client-registry.clients
    @emit 'routing-setup'
    'success'

  # registers the service with the given data
  # as a sender and receiver of messages
  add-routing-config: (routing-config) ~>
    @client-registry.add-routing-config routing-config
    @message-sender.bind-new-service service-name: routing-config.name, host: routing-config.host, port: routing-config.port
    'success'

  # deregisters a service with the given data
  # as a sender and receiver of messages
  remove-routing-config: ({service-name, host}) ~>
    @client-registry.remove-routing-config {service-name, host}
    @message-sender.unbind-service {service-name, host}
    'success'

  # sends the given message to all subscribers of it.
  send-message: (message-data) ~>
    # convert the outgoing message name from its internal version to the public version
    sender = @client-registry.clients[message-data.sender]
    external-message-name = @client-registry.outgoing-message-name message-data.name, sender
    message-data.original-name = message-data.name
    message-data.name = external-message-name
    message-data.timestamp = nanoseconds process.hrtime!
    # determine the subscribers
    subscribers = @client-registry.subscribers-to external-message-name
    subscriber-names = [subscriber.name for subscriber in subscribers]

    # calculate a message's response time if it is a reply
    if message-data.response-to
      original-timestamp  = @message-cache.get-original-timestamp message-data.id
      message-data.response-time = message-data.timestamp - original-timestamp
    else
      @message-cache.push message-data.id, message-data.timestamp

    # send the message to the subscribers
    debug "sending '#{message-data.name}' to #{subscriber-names}"
    sent-messages = @message-sender.send-to-services message-data, subscribers
    @emit 'message', messages: sent-messages, receivers: subscriber-names

    'success'



module.exports = ExoCom
