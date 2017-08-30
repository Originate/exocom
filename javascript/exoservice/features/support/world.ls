require! {
  'dim-console'
  'observable-process' : ObservableProcess
  'path'
  'prelude-ls' : {any}
  'wait' : {wait-until}
}


observableProcessOptions = if process.env.DEBUG_EXOSERVICE
  verbose: yes
  stdout: process.stdout
  stderr: process.stderr
else
  verbose: no
  stdout: no
  stderr: no


World = !->

  @create-exoservice-instance = ({role, exocom-port}, done) ->
    fixturePath = path.join(process.cwd!, 'features', 'fixtures', role)
    @process = new ObservableProcess("./node_modules/.bin/lsc #{fixturePath}",
                                     env: {EXOCOM_PORT: exocom-port, ROLE: role},
                                     verbose: observableProcessOptions.verbose,
                                     stdout: observableProcessOptions.stdout,
                                     stderr: observableProcessOptions.stderr)
      ..wait 'online at port', done


  @remove-register-service-message = (@exocom, done) ->
    wait-until (~> @exocom.received-messages.length), 10, ~>
      @exocom.reset! if @exocom.received-messages |> any (.name is 'exocom.register-service')
      done!

module.exports = World
