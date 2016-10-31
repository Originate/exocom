require! {
  'async'
  'chai' : {expect}
  'dim-console'
  'jsdiff-console'
  './mock-service': MockService
  'nitroglycerin' : N
  'observable-process' : ObservableProcess
  'record-http' : HttpRecorder
  'request'
  './text-tools' : {ascii}
  'wait' : {wait-until, wait}
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exocom-instance = ({exocom-websockets-port, exocom-http-port, service-messages = '{}'}, done) ->
    env =
      EXOCOM_HTTP_PORT : exocom-http-port
      EXOCOM_WEBSOCKETS_PORT : exocom-websockets-port
      SERVICE_MESSAGES : service-messages
    @exocom-http-port = exocom-http-port
    @exocom-websockets-port = exocom-websockets-port
    @process = new ObservableProcess "bin/exocom", stdout: dim-console.process.stdout, stderr: dim-console.process.stderr, env: env
      ..wait "WebSocket listener online at port #{@exocom-websockets-port}", done


  @create-mock-service-at-port = ({name, port, namespace}, done) ->
    (@service-mocks or= {})[name] = new MockService {port, name, namespace}
    wait 200, done


  @run-exocom-at-port = (ports, _expect-error, done) ->
    env =
      EXOCOM_HTTP_PORT : ports.http-port
      EXOCOM_WEBSOCKETS_PORT : ports.websockets-port
    @process = new ObservableProcess "bin/exocom", stdout: dim-console.process.stdout, stderr: dim-console.process.stderr, env: env
    done!


  @service-sends-message = ({service, message}, done) ->
    request-data =
      sender: service
      payload: ''
      id: '123'
      name: message
    @service-mocks[service].send request-data
    done!


  @service-sends-reply = ({service, message, response-to}, done) ->
    request-data =
      sender: service
      payload: ''
      id: '123'
      response-to: response-to
      name: message
    @service-mocks[service].send request-data
    done!



  @set-service-landscape = (service-data, done) ->
    request-data =
      url: "http://localhost:#{@exocom-http-port}/services"
      method: 'POST'
      body: service-data
      json: yes
    request request-data, N (response) ->
      expect(response.status-code).to.equal 200
      done!


  @verify-abort-with-message = (message, done) ->
    @process.wait message, ~>
      wait-until (~> @process.ended), done


  @verify-exocom-broadcasted-message = ({message, sender, receivers}, done) ->
    @process.wait "#{sender} is broadcasting '#{message}' to the #{receivers.join ', '}", done


  @verify-exocom-signaled-string = (message, done) ->
    [...message-main, response-time-msg] = message.split '  '
    @process.wait "#{message-main.join '  '}  ", done


  @verify-exocom-received-message = (message, done) ->
    @process.wait "broadcasting '#{message}'", done


  @verify-exocom-received-reply = (message, done) ->
    @process.wait "broadcasting '#{message}'", done


  @verify-routing-setup = (expected-routing, done) ->
    request "http://localhost:#{@exocom-http-port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).routes, expected-routing, done


  @verify-listening-at-ports = (ports, done) ->
    messages = []
    messages.push "WebSocket listener online at port #{ports.websockets-port}" if ports.websockets-port
    messages.push "HTTP service online at port #{ports.http-port}" if ports.http-port
    async.each messages,
               ((message, cb) ~> @process.wait message, cb),
               done


  @verify-sent-calls = ({service-name, message, id = '123', response-to}, done) ->
    wait-until (~> @service-mocks[service-name].received-messages[0]?.name is message), 1, ~>
      expected =
        name: message
        id: id
        payload: ''
      timestamp = @service-mocks[service-name].received-messages[0].timestamp
      response-time = @service-mocks[service-name].received-messages[0].response-time
      expected.timestamp = timestamp if timestamp
      expected.response-time = response-time if response-time
      expected.response-to = response-to if response-to
      jsdiff-console @service-mocks[service-name].received-messages[0], expected, done


  @verify-service-setup = (service-data, done) ->
    request "http://localhost:#{@exocom-http-port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).services, service-data, done



module.exports = ->
  @World = CliWorld if process.env.EXOCOM_TEST_DEPTH is 'CLI'
