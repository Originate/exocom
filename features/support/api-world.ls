require! {
  '../..' : ExoService
  'path'
}


# Provides steps for testing against the JS API
ApiWorld = !->

  @create-exoservice-instance = ({service-name, exorelay-port, exocom-port}, done) ->
    @exoservice = new ExoService {root: path.join('features', 'example-apps', service-name), exorelay-port, exocom-port, service-name}
      ..on 'online', (port) -> done!
      ..listen!


module.exports = ->
  @World = ApiWorld if process.env.EXOSERVICE_TEST_DEPTH is 'API'
