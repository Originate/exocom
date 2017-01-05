require! {
  'jsonic'
  'remove-value'
  'require-yaml'
  './subscription-manager' : SubscriptionManager
}


class ClientRegistry

  ({service-routes = '{}'} = {}) ->

    # List of messages that are received by the applications services
    #
    # the format is:
    # {
    #   'role 1':
    #     receives: ['message 1', 'message 2']
    #     sends: ['message 3', 'message 4']
    #     internal-namespace: 'my internal namespace'
    #   'role 2':
    #     ...
    @routing = @_parse-service-routes service-routes

    # The main list of clients that are currently registered
    #
    # The format is:
    # {
    #   'client 1 name':
    #     client-name: ...
    #     service-type: ...
    #     namespace: ...
    #   'client 2 name':
    #     ...
    @clients = {}

    @subscriptions = new SubscriptionManager @routing



  # registers the given service instance that just came online
  register-client: (client) ->
    @clients[client.client-name] =
      client-name: client.client-name
      service-type: client.client-name
      internal-namespace: @routing[client.client-name].internal-namespace

    @subscriptions.add-all client-name: client.client-name, service-type: client.client-name


  # deregisters a service instance that went offline
  deregister-client: (client-name) ->
    @subscriptions.remove client-name
    delete @clients[client-name]


  # Returns the clients that are subscribed to the given message
  subscribers-for: (message-name) ->
    @subscriptions.subscribers-for message-name


  # returns whether the given sender is allowed to send messages with the given name
  can-send: (sender, message-name) ->
    @routing[sender].sends |> (.includes message-name)


  # Returns the external name for the given message sent by the given service,
  # i.e. how the sent message should appear to the other services.
  outgoing-message-name: (message-name, service) ->
    message-parts = message-name.split '.'
    switch
    | message-parts.length is 1                       =>  message-name
    | message-parts[0] is service.internal-namespace  =>  "#{service.service-type}.#{message-parts[1]}"
    | otherwise                                       =>  message-name


  _parse-service-routes: (service-routes) ->
    result = {}
    for service-route in jsonic(service-routes)
      result[service-route.role] =
        receives: service-route.receives
        sends: service-route.sends
        internal-namespace: service-route.namespace
    result



module.exports = ClientRegistry
