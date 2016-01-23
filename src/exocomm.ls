require! {
  'events' : {EventEmitter}
  './http-listener' : HttpListener
  './client-registry' : ClientRegistry
  'rails-delegate' : {delegate, delegate-event}
}


class ExoComm extends EventEmitter

  ->
    @http-listener = new HttpListener
      ..on 'register-service', @register-service
      ..on 'get-config', @get-config
    delegate 'close' 'port', from: @, to: @http-listener
    delegate-event 'listening' 'error', from: @http-listener, to: @

    @client-registry = new ClientRegistry


  get-config: (done) ~>
    done clients: @client-registry.clients!


  listen: (port) ->
    @http-listener.listen port || 3100



  # registers the service with the given data
  register-service: (service-data) ~>
    @client-registry.register service-data
    'success'



module.exports = ExoComm
