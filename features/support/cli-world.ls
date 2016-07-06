require! {
  'async'
  'chai' : {expect}
  'jsdiff-console'
  './my-console'
  './mock-service': MockService
  'nitroglycerin' : N
  'observable-process' : ObservableProcess
  'record-http' : HttpRecorder
  'request'
  './text-tools' : {ascii}
  'wait' : {wait-until}
}


# Provides steps for end-to-end testing of the service as a stand-alone binary
CliWorld = !->

  @create-exocom-instance = (ports, done) ->
    @exocom-http-port = ports.http-port
    @exocom-zmq-port = ports.zmq-port
    @process = new ObservableProcess "bin/exocom --zmq-port #{@exocom-zmq-port} --http-port #{@exocom-http-port}", console: my-console
      ..wait "HTTP service online at port #{@exocom-http-port}", done


  @create-instance-at-port = (name, port, done) ->
    (@service-mocks or= {})[name] = new MockService push-port: @exocom-zmq-port, pull-port: port
    done!


  @run-exocom-at-port = (ports, _expect-error, done) ->
    command = "bin/exocom"
    command += " --zmq-port #{ports.zmq-port}" if ports.zmq-port
    command += " --http-port #{ports.http-port}" if ports.http-port
    @process = new ObservableProcess command, console: my-console
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
    @process.wait message, done


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
    messages.push "ZMQ service online at port #{ports.zmq-port}" if ports.zmq-port
    messages.push "HTTP service online at port #{ports.http-port}" if ports.http-port
    async.each messages,
               ((message, cb) ~> @process.wait message, cb),
               done


  @verify-sent-calls = ({service-name, message, id = '123', response-to}, done) ->
    wait-until (~> @service-mocks[service-name].received-messages[0]), 1, ~>
      expected =
        name: message
        id: id
        payload: ''
      expected.response-to = response-to if response-to
      jsdiff-console @service-mocks[service-name].received-messages[0], expected, done


  @verify-service-setup = (service-data, done) ->
    request "http://localhost:#{@exocom-http-port}/config.json", (err, result, body) ->
      expect(err).to.be.null
      expect(result.status-code).to.equal 200
      jsdiff-console JSON.parse(body).services, service-data, done



module.exports = ->
  @World = CliWorld if process.env.EXOCOM_TEST_DEPTH is 'CLI'
