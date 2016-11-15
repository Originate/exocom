require! {
  '../..' : ExoService
  'path'
  'prelude-ls' : {any}
  'wait' : {wait-until}
}


# Provides steps for testing against the JS API
ApiWorld = !->

  @create-exoservice-instance = ({service-name, exocom-port}, done) ->
    @exoservice = new ExoService {
      root: path.join('features', 'example-services', service-name)
      exocom-host: "localhost"
      exocom-port
      service-name
      }
      ..on 'online', (port) -> done!
      ..listen!


  @remove-register-service-message = (@exocom, done) ->
    wait-until (~> @exocom.received-messages.length), 10, ~>
      @exocom.reset! if @exocom.received-messages |> any (.name is 'exocom.register-service')
      done!


module.exports = ->
  @World = ApiWorld if process.env.EXOSERVICE_TEST_DEPTH is 'API'
