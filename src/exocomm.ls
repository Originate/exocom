require! {
  'events' : {EventEmitter}
  './http-listener' : HttpListener
  './client-registry' : ClientRegistry
  './message-sender' : MessageSender
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exocomm')


class ExoComm extends EventEmitter

  ->
    @http-listener = new HttpListener
      ..on 'set-services', @set-services
      ..on 'send-message', @send-message
      ..on 'get-config', @get-config
    @client-registry = new ClientRegistry
    @message-sender = new MessageSender

    delegate 'close' 'port', from: @, to: @http-listener
    delegate-event 'listening' 'error', from: @http-listener, to: @


  # returns the current configuration of this ExoComm instance
  get-config: ~>
    {
      services: @client-registry.clients
      routes: @client-registry.routes
    }


  # takes this instance online at the given port
  listen: (port) ->
    @http-listener.listen port
    debug "listening at port #{port}"


  # registers the service with the given data
  # as a sender and receiver of messages
  set-services: (service-data) ~>
    debug 'receiving service data'
    @emit 'routing-setup'
    @client-registry.set-services service-data
    'success'


  # sends the given message to all subscribers of it.
  send-message: (message-data) ~>
    subscribers = @client-registry.subscribers-to message-data.name
    subscriber-names = [subscriber.name for subscriber in subscribers]
    debug "sending '#{message-data.name}' to #{subscriber-names}"
    @message-sender.send-to-services message-data, subscribers
    @emit 'message', message-data.name, subscriber-names
    'success'



module.exports = ExoComm
