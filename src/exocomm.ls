require! {
  'events' : {EventEmitter}
  './http-listener' : HttpListener
  './client-registry' : ClientRegistry
  './command-sender' : CommandSender
  'rails-delegate' : {delegate, delegate-event}
}
debug = require('debug')('exocomm')


class ExoComm extends EventEmitter

  ->
    @http-listener = new HttpListener
      ..on 'set-services', @set-services
      ..on 'send-command', @send-command
      ..on 'get-config', @get-config
    @client-registry = new ClientRegistry
    @command-sender = new CommandSender

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
  # as a sender and receiver of commands
  set-services: (service-data) ~>
    debug 'receiving service data'
    @emit 'routing-setup'
    @client-registry.set-services service-data
    'success'


  # sends the given command to all subscribers of it.
  send-command: (command-data) ~>
    subscribers = @client-registry.subscribers-to command-data.name
    subscriber-names = [subscriber.name for subscriber in subscribers]
    debug "sending '#{command-data.name}' to #{subscriber-names}"
    @command-sender.send-to-services command-data, subscribers
    @emit 'command', command-data.name, subscriber-names
    'success'



module.exports = ExoComm
