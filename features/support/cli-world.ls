require! {
  'observable-process' : ObservableProcess
  'path'
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exoservice-instance = ({service-name, exorelay-port, exocomm-port}, done) ->
    @process = new ObservableProcess("bin/exo-js run --exorelay-port #{exorelay-port} --exocomm-port #{exocomm-port}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port', done



module.exports = ->
  @World = CliWorld if process.env.EXOCOMM_TEST_DEPTH is 'CLI'
