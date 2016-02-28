require! {
  'remove-value'
}


class ClientRegistry

  ->

    # List of clients that are currently registered
    #
    # The format is:
    # {
    #   'client 1 name':
    #     host: ...
    #     port: ...
    #   'client 2 name':
    #     ...
    @clients = {}

    # List of clients that are subscribed to the given message
    #
    # The format is:
    # {
    #   'message 1 name':
    #     receivers:
    #       * name: ...
    #         host: ...
    #         port: ...
    #       * name: ...
    #   'message 2 name':
    #     ...
    @routes = {}


  reset: ->
    @clients = {}
    @subscribers = {}


  # Sets the currently known service landscape to the given setup
  set-services: (data) ->
    @reset!
    for service in data
      @clients[service.name] =
        host: service.host
        port: service.port
      for message in service.receives
        @routes[message] or= {}
        @routes[message].receivers or= []
        @routes[message].receivers.push do
          name: service.name
          host: service.host
          port: service.port


  # Returns the clients that are subscribed to the given message
  subscribers-to: (message-name) ->
    | !@routes[message-name]  =>  throw new Error "No receivers for message '#{message-name}' registered"
    @routes[message-name].receivers



module.exports = ClientRegistry
