require! {
  'remove-value'
}


class ClientRegistry

  ->

    # List of clients that are currently registered
    #
    # The format is:
    # 'client name':
    #   name: 'client name'
    #   sends: ['command 1', 'command 2']
    #   receives: ['command 3']
    @_clients = {}

    # List of clients that are subscribed to the given command
    #
    # The format is:
    # 'command name': [port, port]
    @_subscribers = {}


  # Returns data for the client with the given name
  client: (name) ->
    @_clients[name]


  clients: ->
    @_client-data Object.keys(@_clients)


  register: (service) ->
    @remove-service service.name
    @_clients[service.name] = service
    for command in service.receives
      (@_subscribers[command] ||= []).push service.name


  # Removes the service with the given name from the list of services
  remove-service: (service-name) ->
    return unless (service = @client service-name)
    for command in service.receives
      remove-value @_subscribers[command], service-name
    delete @_clients[service-name]


  # Returns the names of the clients
  # that are subscribed to the command with the given name
  subscriber-names-to: (command-name) ->
    @_subscribers[command-name] or= []


  # Returns the clients that are subscribed to the given command
  subscribers-to: (command-name) ->
    @_client-data @subscriber-names-to(command-name)


  # Returns an array with full data for the clients with the given names
  _client-data: (client-names) ->
    [@client(name) for name in client-names]


module.exports = ClientRegistry
