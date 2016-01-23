require! {
  'events' : {EventEmitter}
  'eventualize'
  './http-listener' : HttpListener
  './client-registry' : ClientRegistry
  'rails-delegate' : {delegate, delegate-event}
}


class ExoComm extends EventEmitter

  ->
    @http-listener = new HttpListener
    @client-registry = new ClientRegistry

    delegate 'close' 'port', from: @, to: @http-listener
    delegate-event 'listening' 'error', from: @http-listener, to: @

    eventualize this


  listen: (port) ->
    @http-listener.listen port || 3100


  on-http-listener-get-config: (done) ~>
    done clients: @client-registry.clients!


  # registers the service with the given data
  on-http-listener-register-service: (service-data) ~>
    @client-registry.register service-data
    'success'



module.exports = ExoComm
