require! {
  'jsonic'
  'remove-value'
  'require-yaml'
}


class ClientRegistry

  ({@service-messages = '{}'} = {}) ->

    # List of messages that are received by the applications services
    @receives = {[service.name, service.receives] for service in jsonic(@service-messages)}

    # List of clients that are currently registered
    #
    # The format is:
    # {
    #   'client 1 name':
    #     host: ...
    #     port: ...
    #     name: ...
    #     namespace: ...
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
  set-routing-config: (services) ->
    @reset!
    for service in services
      @add-routing-config service


  # Adds service routing configurations to the given setup
  add-routing-config: (service) ->
    @clients[service.name] =
      host: service.host
      port: service.port
      name: service.name
      type: service.type
      internal-namespace: service.internal-namespace
    for message in (@receives[service.name] or {})
      external-message = @external-message-name {message, service-name: service.name, internal-namespace: service.internal-namespace}
      @routes[external-message] or= {}
      @routes[external-message].receivers or= []
      @routes[external-message].receivers.push do
        name: service.name
        host: service.host
        port: service.port
        internal-namespace: service.internal-namespace


  remove-routing-config: ({service-name, host}) ->
    for message in (@receives[service-name] or {})
      external-message = @external-message-name {message, service-name, internal-namespace: @clients[service-name].internal-namespace}
      delete @routes[external-message]
    delete @clients[service-name]


  # Returns the clients that are subscribed to the given message
  subscribers-to: (message-name) ->
    | !@routes[message-name]  =>  throw new Error "No receivers for message '#{message-name}' registered"
    @routes[message-name].receivers


  # Returns the message name to which the given service would have to subscribe
  # if it wanted to receive the given message expressed in its internal form.
  #
  # Example:
  # - service "tweets" has internal namespace "text-snippets"
  # - it only knows the "text-snippets.create" message
  # - the external message name that it has to subscribe to is "tweets.create"
  external-message-name: ({message, service-name, internal-namespace}) ->
    message-parts = message.split '.'
    switch
    | !internal-namespace               =>  message
    | message-parts.length is 1         =>  message
    | message-parts[0] is service-name  =>  message
    | otherwise                         =>  "#{service-name}.#{message-parts[1]}"


  # Returns the external name for the given message sent by the given service,
  # i.e. how the sent message should appear to the other services.
  #
  # Example:
  # - service "tweets" has internal name "text-snippets"
  # - it sends the message "text-snippets.created" to exocom
  # - exocom converts this message to "tweets.created"
  outgoing-message-name: (message, service) ->
    message-parts = message.split '.'
    switch
    | message-parts.length is 1                       =>  message
    | message-parts[0] is service.internal-namespace  =>  "#{service.name}.#{message-parts[1]}"
    | otherwise                                       =>  message



module.exports = ClientRegistry
