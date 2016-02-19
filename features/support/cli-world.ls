require! {
  'chai' : {expect}
  'jsdiff-console'
  './my-console'
  'nitroglycerin' : N
  'observable-process' : ObservableProcess
  'record-http' : HttpRecorder
  'request'
  './text-tools' : {ascii}
  'wait' : {wait-until}
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exocomm-instance = ({port}, done) ->
    @process = new ObservableProcess "bin/exocomm --port #{@exocomm-port}", verbose: yes, console: my-console
      ..wait "online at port #{port}", done


  @create-instance-at-port = (name, port, done) ->
    @receivers or= {}
    @receivers[name] = new HttpRecorder
      ..listen port, done


  @run-exocomm-at-port = (port, _expect-error, done) ->
    @process = new ObservableProcess "bin/exocomm --port #{port}", verbose: yes, console: my-console
    done!


  @service-sends-message = ({service, message}, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/send/#{message}",
      method: 'POST'
      body:
        sender: service
        payload: ''
        request-id: '123'
      json: yes
    request request-data, done


  @service-sends-reply = ({service, message, request-id}, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/send/#{message}",
      method: 'POST'
      body:
        sender: service
        payload: ''
        request-id: '123'
        response-to: request-id
      json: yes
    request request-data, done


  @set-service-landscape = (service-data, done) ->
    request-data =
      url: "http://localhost:#{@exocomm-port}/services"
      method: 'POST'
      body: service-data
      json: yes
    request request-data, N (response) ->
      expect(response.status-code).to.equal 200
      done!


  @verify-abort-with-message = (message, done) ->
    @process.wait message, ~>
      wait-until (~> @process.crashed), done


  @verify-exocomm-broadcasted-message = ({message, sender, receivers}, done) ->
    @process.wait "#{sender} is broadcasting '#{message}' to the #{receivers.join ', '}", done


  @verify-exocomm-received-message = (message, done) ->
    @process.wait "broadcasting '#{message}'", done


  @verify-exocomm-received-reply = (message, done) ->
    @process.wait "broadcasting '#{message}'", done


  @verify-routing-setup = (expected-routing, done) ->
    request "http://localhost:#{@exocomm-port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).routes, expected-routing, done


  @verify-runs-at-port = (port, done) ->
    @process.wait "online at port #{port}", done


  @verify-sent-calls = ({service-name, message, response-to}, done) ->
    service-receiver = @receivers[service-name]
    condition = -> service-receiver.calls.length is 1
    wait-until condition, 10, ~>
      expected = [
        url: "http://localhost:#{@ports[service-name]}/run/#{message}"
        method: 'POST'
        body:
          requestId: '123'
          payload: ''
        headers:
          accept: 'application/json'
          'content-type': 'application/json'
      ]
      expected[0].body.response-to = response-to if response-to
      jsdiff-console service-receiver.calls, expected, done


  @verify-service-setup = (service-data, done) ->
    request "http://localhost:#{@exocomm-port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).services, service-data, done


module.exports = ->
  @World = CliWorld if process.env.EXOCOMM_TEST_DEPTH is 'CLI'
