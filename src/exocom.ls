require! {
  'events' : {EventEmitter}
  './http-listener/http-listener' : HttpListener
  './client-registry/client-registry' : ClientRegistry
  './message-sender/message-sender' : MessageSender
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exocom')


class ExoCom extends EventEmitter

  ->
    @http-listener = new HttpListener @
    @client-registry = new ClientRegistry
    @message-sender = new MessageSender

    delegate 'close' 'listen' 'port', from: @, to: @http-listener
    delegate-event 'listening' 'error', from: @http-listener, to: @


  # returns the current configuration of this ExoCom instance
  get-config: ~>
    {
      services: @client-registry.clients
      routes: @client-registry.routes
    }


  # registers the service with the given data
  # as a sender and receiver of messages
  set-routing-config: (routing-config) ~>
    debug 'receiving routing config'
    @client-registry.set-routing-config routing-config
    @emit 'routing-setup'
    'success'


  # sends the given message to all subscribers of it.
  send-message: (message-data) ~>

    # convert the outgoing message name from its internal version to the public version
    sender = @client-registry.clients[message-data.sender]
    external-message-name = @client-registry.outgoing-message-name message-data.name, sender
    message-data.original-name = message-data.name
    message-data.name = external-message-name

    # determine the subscribers
    subscribers = @client-registry.subscribers-to external-message-name
    subscriber-names = [subscriber.name for subscriber in subscribers]

    # send the message to the subscribers
    debug "sending '#{message-data.name}' to #{subscriber-names}"
    sent-messages = @message-sender.send-to-services message-data, subscribers
    @emit 'message', messages: sent-messages, receivers: subscriber-names

    'success'



module.exports = ExoCom
