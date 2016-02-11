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

    # List of clients that are subscribed to the given command
    #
    # The format is:
    # {
    #   'command 1 name':
    #     receivers:
    #       * name: ...
    #         host: ...
    #         port: ...
    #       * name: ...
    #   'command 2 name':
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
      for command in service.receives
        @routes[command] or= {}
        @routes[command].receivers or= []
        @routes[command].receivers.push do
          name: service.name
          host: service.host
          port: service.port


  # Returns the clients that are subscribed to the given command
  subscribers-to: (command-name) ->
    @routes[command-name].receivers



module.exports = ClientRegistry
