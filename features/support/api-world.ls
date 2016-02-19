require! {
  '../..' : ExoService
  'path'
}


# Provides steps for testing against the JS API
ApiWorld = !->

  @create-exoservice-instance = ({service-name, exorelay-port, exocomm-port}, done) ->
    @exoservice = new ExoService {root: path.join('features', 'example-apps', service-name), exorelay-port, exocomm-port, service-name}
      ..on 'online', (port) -> done!
      ..listen!



module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
