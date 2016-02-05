require! {
  'observable-process' : ObservableProcess
  'path'
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exoservice-instance = ({service-name, port, exocomm-port}, done) ->
    command-line = 'bin/exo-js run'
    if port then command-line += " --port #{port}"
    if exocomm-port then command-line += " --exocomm-port #{exocomm-port}"
    @process = new ObservableProcess(command-line,
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port', done



module.exports = ->
  @World = CliWorld if process.env.EXOCOMM_TEST_DEPTH is 'CLI'
