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
World = !->

  @create-exocom-instance = ({port, service-routes = '{}'}, done) ->
    env =
      PORT : port
      SERVICE_ROUTES : service-routes
    @port = port
    @process = new ObservableProcess "bin/exocom", stdout: dim-console.process.stdout, stderr: dim-console.process.stderr, env: env
      ..wait "WebSocket listener online at port #{@port}", done


  @create-mock-service-at-port = ({client-name, port, namespace}, done) ->
    (@service-mocks or= {})[client-name] = new MockService {port, client-name, namespace}
    @service-mocks[client-name].connect {}, ->
      wait 200, done


  @run-exocom-at-port = (port, _expect-error, done) ->
    env =
      PORT : port
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
    request "http://localhost:#{@port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).routes, expected-routing, done


  @verify-listening-at-ports = (port, done) ->
    messages = []
    messages.push "WebSocket listener online at port #{port}" if port
    messages.push "HTTP service online at port #{port}" if port
    async.each messages,
               ((message, cb) ~> @process.wait message, cb),
               done


  @verify-sent-calls = ({client-name, message-name, id = '123', response-to}, done) ->
    service = @service-mocks[client-name]
    wait-until (~> service.received-messages[0]?.name is message-name), 1, ~>
      expected =
        name: message-name
        id: id
        payload: ''
      received-message = service.received-messages[0]
      expected.timestamp = that if received-message.timestamp
      expected.response-time = that if received-message.response-time
      expected.response-to = that if response-to
      jsdiff-console received-message, expected, done


  @verify-service-setup = (service-data, done) ->
    request "http://localhost:#{@port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).clients, service-data, done



module.exports = ->
  @World = World
