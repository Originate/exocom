require! {
  './http-listener' : HttpListener
  './client-registry' : ClientRegistry
}


class ExoComm

  ->
    @http-listener = new HttpListener
    @http-listener.on 'register-service', @register-service
    @http-listener.on 'get-config', @get-config

    @client-registry = new ClientRegistry


  get-config: (done) ~>
    done clients: @client-registry.clients!


  listen: (port, done) ->
    port or= 3100
    @http-listener.listen port, ->
      done port


  # registers the service with the given data
  register-service: (service-data) ~>
    @client-registry.register service-data
    'success'



module.exports = ExoComm
