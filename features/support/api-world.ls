require! {
  '../..' : ExoService
  'path'
}


# Provides steps for testing against the JS API
ApiWorld = !->

  @create-exoservice-instance = ({service-name, port, exocomm-port}, done) ->
    @exoservice = new ExoService {root: path.join('features', 'example-apps', service-name), exocomm-port}
      ..on 'online', (port) -> done!
      ..listen {port}



module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
