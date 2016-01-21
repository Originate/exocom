class ClientRegistry

  ->

    # List of clients that are currently registered.
    # The format is:
    # 'client name':
    #   name: 'client name'
    #   sends: ['command 1', 'command 2']
    #   receives: ['command 3']
    @_clients = {}


  clients: ->
    [@_clients[key] for key, _ of @_clients]


  register: (service-data) ->
    @remove-service service-data.name
    @_clients[service-data.name] = service-data


  # Removes the given service from the list of services
  remove-service: (service-name) ->
    delete @_clients[service-name]



module.exports = ClientRegistry
