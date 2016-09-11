require! {
  'dim-console'
  'observable-process' : ObservableProcess
  'path'
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exoservice-instance = ({service-name, exorelay-port, exocom-port}, done) ->
    command = "#{process.cwd!}/bin/exo-js"
    if process.platform is 'win32' then command += '.cmd'
    @process = new ObservableProcess(command,
                                     env: {SERVICE_NAME: service-name, EXORELAY_PORT: exorelay-port, EXOCOM_PORT: exocom-port},
                                     cwd: path.join(process.cwd!, 'features', 'example-services', service-name),
                                     verbose: yes,
                                     stdout: process.stdout,
                                     stderr: process.stderr)
      ..wait 'online at port', done


module.exports = ->
  @World = CliWorld if process.env.EXOSERVICE_TEST_DEPTH is 'CLI'
