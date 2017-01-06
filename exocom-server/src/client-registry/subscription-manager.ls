# manages which client is subscribed to which message on the bus
class SubscriptionManager


  (@routing) ->

    # List of clients that are subscribed to the given message
    #
    # The format is:
    # {
    #   'message 1 name': [
    #     * name: 'client 1 name'
    #       internal-namespace: 'my namespace'
    #     * name: ...
    #   ],
    #   'message 2 name':
    #     ...
    @subscribers = {}


  # adds subscriptions for the client with the given name
  add-all: ({client-name, service-type}) ->
    for internal-message-name in @routing[service-type].receives or {}
      @add {internal-message-name, client-name}


  # Adds the given client to the subscription list for the given message
  add: ({internal-message-name, client-name}) ->
    public-message-name = @public-message-name {internal-message-name, client-name, internal-namespace: @routing[client-name].internal-namespace}
    (@subscribers[public-message-name] or= []).push do
      client-name: client-name
      internal-namespace: @routing[client-name].internal-namespace


  remove: (client-name) ->
    for internal-message-name in (@routing[client-name].receives or {})
      public-message-name = @public-message-name {internal-message-name, client-name, internal-namespace: @subscribers[client-name].internal-namespace}
      # TODO: this is broken, make this remove only the client
      delete @subscribers[public-message-name]


  subscribers-for: (message-name) ->
    @subscribers[message-name]


  # Returns the message name to which the given service would have to subscribe
  # if it wanted to receive the given message expressed in its internal form.
  #
  # Example:
  # - service "tweets" has internal namespace "text-snippets"
  # - it only knows the "text-snippets.create" message
  # - the external message name that it has to subscribe to is "tweets.create"
  public-message-name: ({internal-message-name, client-name, internal-namespace}) ->
    message-parts = internal-message-name.split '.'
    switch
    | !internal-namespace              =>  internal-message-name
    | message-parts.length is 1        =>  internal-message-name
    | message-parts[0] is client-name  =>  internal-message-name
    | otherwise                        =>  "#{client-name}.#{message-parts[1]}"



module.exports = SubscriptionManager
