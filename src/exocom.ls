require! {
  './client-registry/client-registry' : ClientRegistry
  'events' : {EventEmitter}
  './listener-subsystem/listener-subsystem' : ListenerSubsystem
  './message-sender/message-sender' : MessageSender
  'process'
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exocom')


class ExoCom extends EventEmitter

  ->
    @listener-subsystem = new ListenerSubsystem @
    @client-registry = new ClientRegistry
    @message-sender = new MessageSender

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


  # registers the service with the given data
  # as a sender and receiver of messages
  set-routing-config: (routing-config) ~>
    debug 'receiving service setup'
    @client-registry.set-routing-config routing-config
    @message-sender.bind-services @client-registry.clients
    @emit 'routing-setup'
    'success'


  # sends the given message to all subscribers of it.
  send-message: (message-data) ~>
    [seconds, nanos] = process.hrtime!
    # convert the outgoing message name from its internal version to the public version
    sender = @client-registry.clients[message-data.sender]
    external-message-name = @client-registry.outgoing-message-name message-data.name, sender
    message-data.original-name = message-data.name
    message-data.name = external-message-name
    message-data.timestamp = seconds * 1e9 + nanos

    # determine the subscribers
    subscribers = @client-registry.subscribers-to external-message-name
    subscriber-names = [subscriber.name for subscriber in subscribers]

    # send the message to the subscribers
    debug "sending '#{message-data.name}' to #{subscriber-names}"
    sent-messages = @message-sender.send-to-services message-data, subscribers
    @emit 'message', messages: sent-messages, receivers: subscriber-names

    'success'



module.exports = ExoCom
