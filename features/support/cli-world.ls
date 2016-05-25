require! {
  'dim-console'
  'observable-process' : ObservableProcess
  'path'
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exoservice-instance = ({service-name, exorelay-port, exocom-port}, done) ->
    @process = new ObservableProcess("#{process.cwd!}/bin/exo-js"
                                     env: {SERVICE_NAME: service-name, EXORELAY_PORT: exorelay-port, EXOCOM_PORT: exocom-port},
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: yes,
                                     console: dim-console.console)
      ..wait 'online at port', done



module.exports = ->
  @World = CliWorld if process.env.EXOSERVICE_TEST_DEPTH is 'CLI'
