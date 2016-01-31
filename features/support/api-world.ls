require! {
  '../../lib/exocomm' : ExoComm
  './text-tools' : {ascii}
  'chai' : {expect}
  'jsdiff-console'
  'record-http' : HttpRecorder
  'wait' : {wait, wait-until}
}


ApiWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @exocomm = new ExoComm
      ..listen port
      ..on 'listening', -> done!


  @instance-for-receiving = ({name, command}, done) ->
    @receivers or= {}
    @receivers[name] = new HttpRecorder
      ..listen 3000 + ascii(name), done
    @exocomm.register-service do
      name: name
      host: 'localhost'
      port: 3000 + ascii(name)
      sends: []
      receives: [command]



  @instance-for-sending = ({name, command}, done) ->
    result = @exocomm.register-service do
      name: name
      host: 'localhost'
      port: 3000 + ascii(name)
      sends: [command]
      receives: []
    expect(result).to.equal 'success'
    done!


  @register-service = (service-data, done) ->
    result = @exocomm.register-service service-data[0]
    expect(result).to.equal 'success'
    done!


  @run-exocomm = (expect-error, done) ->
    @exocomm = new ExoComm
      ..listen null
    if expect-error
      @exocomm.on 'error', (@err) ~> done!
    else
      @exocomm.on 'listening', -> done!


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


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@err).to.equal message
      done!


  @verify-knows-about-services = (service-data, done) ->
    @exocomm.get-config ({clients}) ->
      jsdiff-console clients, service-data, done


  @verify-runs-at-port = (port, done) ->
    expect(@exocomm.port).to.equal port
    done!


  @verify-sent-calls = ({service-name, message}, done) ->
    service-receiver = @receivers[service-name]
    condition = -> service-receiver.calls.length is 1
    wait-until condition, 10, ~>
      expected = [
        url: "http://localhost:4347/run/#{message}"
        method: 'POST'
        body: {}
        headers:
          accept: 'application/json'
          'content-type': 'application/json'
      ]
      jsdiff-console service-receiver.calls, expected, done


module.exports = ->
  @World = ApiWorld if process.env.EXOCOMM_TEST_DEPTH is 'API'
