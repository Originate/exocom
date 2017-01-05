require! {
  'jsonic'
  'remove-value'
  'require-yaml'
  './subscription-manager' : SubscriptionManager
}


class ClientRegistry

  ({service-messages = '{}'} = {}) ->

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
    @routing = @_parse-service-messages service-messages

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



  # Adds service routing configurations to the given setup
  register-client: (service) ->
    @clients[service.name] =
      client-name: service.name
      service-type: service.name
      internal-namespace: @routing[service.name].internal-namespace

    @subscriptions.add-all client-name: service.name, service-type: service.name


  deregister-client: (service-name) ->
    @subscriptions.remove service-name
    delete @clients[service-name]


  # Returns the clients that are subscribed to the given message
  subscribers-for: (message-name) ->
    @subscriptions.subscribers-for message-name


  can-send: (sender, message) ->
    @routing[sender].sends |> (.includes message)


  # Returns the external name for the given message sent by the given service,
  # i.e. how the sent message should appear to the other services.
  outgoing-message-name: (message, service) ->
    message-parts = message.split '.'
    switch
    | message-parts.length is 1                       =>  message
    | message-parts[0] is service.internal-namespace  =>  "#{service.service-type}.#{message-parts[1]}"
    | otherwise                                       =>  message


  _parse-service-messages: (service-messages) ->
    result = {}
    for service in jsonic(service-messages)
      result[service.role] =
        receives: service.receives
        sends: service.sends
        internal-namespace: service.namespace
    result



module.exports = ClientRegistry
