require! {
  '../../dist/exocom' : ExoCom
  './text-tools' : {ascii}
  'chai' : {expect}
  'jsdiff-console'
  './mock-service' : MockService
  'wait' : {wait-until}
}


ApiWorld = !->

  @create-exocom-instance = (ports, done) ->
    @exocom = new ExoCom
      ..on 'zmq-bound', ~> done!
      ..on 'message', ({messages, receivers}) ~>
        @last-sent-messages = messages
        @last-listeners = receivers
      ..listen zmq-port: ports.zmq-port, http-port: ports.http-port


  @create-mock-service-at-port = (name, port, done) ->
    (@service-mocks or= {})[name] = new MockService push-port: null, pull-port: port
    done!


  @run-exocom-at-port = (options, expect-error, done) ->
    @exocom = new ExoCom
    if options.http-port then port-event = 'http-bound' else port-event = 'zmq-bound'
    if expect-error
      @exocom.on 'error', (@err) ~> done!
    else
      @exocom.on port-event , ~> done!
    @exocom.listen zmq-port: (options.zmq-port or 4100), http-port: (options.http-port or 4101)


  @service-sends-message = ({service, message, id = '123'} = {}, done) ->
    result = @exocom.send-message {name: message, sender: service, id}
    expect(result).to.equal 'success'
    done!


  @service-sends-reply = ({service, message, response-to}, done) ->
    result = @exocom.send-message {name: message, sender: service, id: '123', response-to}
    expect(result).to.equal 'success'
    done!


  @set-service-landscape = (service-data, done) ->
    result = @exocom.set-routing-config service-data
    expect(result).to.equal 'success'
    done!


  @verify-abort-with-message = (message, done) ->
    process.next-tick ~>
      expect(@err).to.equal message
      done!


  @verify-exocom-broadcasted-message = ({sender, message, receivers} done) ->
    wait-until (~> @last-sent-messages.name is message), 1, ~>
      expect(@last-sent-messages.sender).to.eql sender
      expect(@last-receivers).to.eql receivers
      done!


  @verify-exocom-broadcasted-reply = (message, done) ->
    wait-until (~> @last-sent-messages.name is message), 1, done


  @verify-exocom-signaled-string = (string, done) ->
    [sender-name, message-names, listeners] = string.split '  '
    message-name-parts = message-names.split ' '
    switch message-name-parts.length
      | 3  =>  [_, translated-name, _] = message-name-parts
      | 5  =>  [_, original-name, _, translated-name, _] = message-name-parts
      | _  =>  throw new Error "Unknown message name parts"
    wait-until (~> @last-sent-messages[0].name is translated-name), 1, ~>
      expect(@last-sent-messages[0].sender).to.eql sender-name
      expect(@last-listeners).to.eql [listeners]
      expect(@last-sent-messages[0].timestamp).to.be.above(0)
      done!


  @verify-routing-setup = (expected-routes, done) ->
    jsdiff-console @exocom.get-config!routes, expected-routes, done


  @verify-exocom-signals-broadcast = (message-name, done) ->
    @exocom.on message-name, (message, receivers) ->
      done!


  @verify-listening-at-ports = ({zmq-port, http-port}, done) ->
    expect(@exocom.zmq-port!).to.equal zmq-port if zmq-port
    expect(@exocom.http-port!).to.equal http-port if http-port
    done!


  @verify-sent-calls = ({service-name, message, id = '123', response-to}, done) ->
    wait-until (~> @service-mocks[service-name].received-messages[0]?.name is message), 1, ~>
      expected =
        name: message
        id: id
      timestamp = @service-mocks[service-name].received-messages[0].timestamp
      expected.timestamp = timestamp if timestamp
      expected.response-to = response-to if response-to
      jsdiff-console @service-mocks[service-name].received-messages[0], expected, done


  @verify-service-setup = (expected-services, done) ->
    jsdiff-console @exocom.get-config!services, expected-services, done



module.exports = ->
  @World = ApiWorld if process.env.EXOCOM_TEST_DEPTH is 'API'
