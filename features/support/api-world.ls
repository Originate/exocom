require! {
  '../../dist/exocomm' : ExoComm
  './text-tools' : {ascii}
  'chai' : {expect}
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait-until}
}


ApiWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @exocomm = new ExoComm
      ..listen port
      ..on 'listening', -> done!
      ..on 'command', (name) ~> @last-received-command = name


  @create-instance-at-port = (name, port, done) ->
    @receivers or= {}
    @receivers[name] = new HttpRecorder
      ..listen port, done
      ..on 'command', (name) ~> @last-received-command = name


  @run-exocomm-at-port = (port, expect-error, done) ->
    @exocomm = new ExoComm
      ..listen port
    if expect-error
      @exocomm.on 'error', (@err) ~> done!
    else
      @exocomm.on 'listening', -> done!


  @service-sends-command = (service, command, done) ->
    result = @exocomm.send-command name: command
    expect(result).to.equal 'success'
    done!


  @set-service-landscape = (service-data, done) ->
    result = @exocomm.set-services service-data
    expect(result).to.equal 'success'
    done!


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@err).to.equal message
      done!


  @verify-exocomm-received-command = (command, done) ->
    wait-until (~> @last-received-command is command), 1, done


  @verify-routing-setup = (expected-routes, done) ->
    jsdiff-console @exocomm.get-config!routes, expected-routes, done


  @verify-runs-at-port = (port, done) ->
    expect(@exocomm.port).to.equal port
    done!


  @verify-sent-calls = ({service-name, message}, done) ->
    service-receiver = @receivers[service-name]
    condition = -> service-receiver.calls.length is 1
    wait-until condition, 10, ~>
      expected = [
        url: "http://localhost:#{@ports[service-name]}/run/#{message}"
        method: 'POST'
        body: {}
        headers:
          accept: 'application/json'
          'content-type': 'application/json'
      ]
      jsdiff-console service-receiver.calls, expected, done


  @verify-service-setup = (expected-services, done) ->
    jsdiff-console @exocomm.get-config!services, expected-services, done



module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
