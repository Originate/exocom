require! {
  'chai' : {expect}
  'jsdiff-console'
  'observable-process' : ObservableProcess
  'record-http' : HttpRecorder
  'request'
  './text-tools' : {ascii}
  'wait' : {wait-until}
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @process = new ObservableProcess "bin/exocomm run --port #{@exocomm-port}", verbose: yes
      ..wait "online at port #{port}", done


  @instance-for-receiving = ({name, command}, done) ->
    @receivers or= {}
    @receivers[name] = new HttpRecorder
      ..listen 3000 + ascii(name), done
    request-data =
      url: "http://localhost:#{@exocomm-port}/register-service",
      method: 'POST'
      body:
        payload:
          name: name
          port: 3000 + ascii(name)
          sends: []
          receives: [command]
      json: yes
    request request-data, done


  @instance-for-sending = ({name, command}, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/register-service",
      method: 'POST'
      body:
        payload:
          name: name
          port: 3000 + ascii(name)
          sends: [command]
          receives: []
      json: yes
    request request-data, done



  @register-service = (service-data, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/register-service",
      method: 'POST'
      body:
        payload: service-data[0]
      json: yes
    request request-data, done


  @run-exocomm = (_expect-error, done) ->
    @process = new ObservableProcess 'bin/exocomm run', verbose: yes
    done!


  @run-exocomm-at-port = (port, _expect-error, done) ->
    @process = new ObservableProcess "bin/exocomm run --port #{port}", verbose: yes
    done!


  @service-sends-command = (service, command, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/send/#{command}",
      method: 'POST'
      body:
        payload: ''
        request-id: '123'
      json: yes
    request request-data, done


  @verify-abort-with-message = (message, done) ->
    @process.wait message, ~>
      wait-until (~> @process.crashed), done


  @verify-knows-about-services = (service-data, done) ->
    request "http://localhost:#{@exocomm-port}/status.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      actual = JSON.parse body
      jsdiff-console actual.clients, service-data, done


  @verify-runs-at-port = (port, done) ->
    @process.wait "online at port #{port}", done


  @verify-sent-calls = ({service-name, message}, done) ->
    service-receiver = @receivers[service-name]
    condition = -> service-receiver.calls.length is 1
    wait-until condition, 10, ->
      expected = [
        url: "http://localhost:4347/run/#{message}"
        method: 'POST'
        body:
          payload: ''
        headers:
          accept: 'application/json'
          'content-type': 'application/json'
      ]
      jsdiff-console service-receiver.calls, expected, done


module.exports = ->
  @World = CliWorld if process.env.EXOCOMM_TEST_DEPTH is 'CLI'
