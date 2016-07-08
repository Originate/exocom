require! {
  '../..' : MockExoCom
  'chai'
  'jsdiff-console'
  'nitroglycerin': N
  'port-reservation'
  'record-http' : HttpRecorder
  'request'
  'sinon'
  'sinon-chai'
  '../support/zmq-endpoint' : ZmqEndpoint
  'wait' : {wait-until}
}
expect = chai.expect
chai.use sinon-chai


module.exports = ->

  @Given /^a listening ExoComMock instance$/, (done) ->
    @exocom = new MockExoCom
    port-reservation.get-port N (@exocom-zmq-port) ~>
      @exocom.listen @exocom-zmq-port
      done!


  @Given /^an ExoComMock instance$/, ->
    @exocom = new MockExoCom


  @Given /^an ExoComMock instance listening at ZMQ port (\d+)$/, (@exocom-zmq-port) ->
    @exocom = new MockExoCom
      ..listen @exocom-zmq-port


  @Given /^a known "([^"]*)" service listening at port (\d+)$/, (name, port) ->
    @exocom.register-service {name, port}
    @service = new ZmqEndpoint push-port: @exocom-zmq-port, pull-port: port


  @Given /^somebody sends it a message$/, ->
    message-data =
      name: "foo"
      payload: ''
      id: '123'
    @service or= new ZmqEndpoint push-port: @exocom-zmq-port
      ..send message-data


  @Given /^somebody sends it a "([^"]*)" message with payload "([^"]*)"$/, (name, payload) ->
    message-data =
      name: name
      payload: payload
      id: '123'
    @service or= new ZmqEndpoint push-port: @exocom-zmq-port
      ..send message-data



  @When /^closing it$/, ->
    @closed = yes
    @exocom.close!


  @When /^I tell it to wait for a call$/, ->
    @call-received = sinon.spy!
    @exocom.on-receive @call-received


  @When /^a call comes in$/, ->
    message-data =
      name: 'foo'
      id: '123'
    @service or= new ZmqEndpoint push-port: @exocom-zmq-port
      ..send message-data


  @When /^trying to send a "([^"]*)" message to the "([^"]*)" service$/, (message-name, service-name) ->
    try
      @exocom.send service: service-name, name: message-name
    catch
      @error = e


  @When /^resetting the ExoComMock instance$/, ->
    @exocom.reset!


  @When /^sending a "([^"]*)" message to the "([^"]*)" service with the payload:$/, (message, service, payload) ->
    @exocom.send service: service, name: message, payload: payload



  @Then /^ExoComMock makes the request:$/, (table, done) ->
    expected-request = table.rows-hash!
    wait-until (~> @service.received-messages.length), 1, ~>
      actual-request = @service.received-messages[0]
      expect(actual-request.name).to.equal expected-request.NAME
      expect(actual-request.payload).to.equal expected-request.PAYLOAD
      done!


  @Then /^I can close it without errors$/, ->
    @exocom.close!


  @Then /^I get the error "([^"]*)"$/, (expected-error) ->
    expect(@error.message).to.equal expected-error


  @Then /^it calls the given callback$/, (done) ->
    wait-until (~> @call-received.called), done


  @Then /^it calls the given callback right away$/, (done) ->
    wait-until (~> @exocom.received-messages.length), 1, ~>
      expect(@call-received).to.have.been.called
      done!


  @Then /^it doesn't call the given callback right away$/, ->
    expect(@call-received).to.not.have.been.called


  @Then /^it has received no messages/, ->
    expect(@exocom.received-messages).to.be.empty


  @Then /^it has received the messages/, (table, done) ->
    wait-until (~> @exocom.received-messages.length), 1, ~>
      expected-messages = [{[key.to-lower-case!, value] for key, value of message} for message in table.hashes!]
      jsdiff-console @exocom.received-messages, expected-messages, done


  @Then /^it is no longer listening at port (\d+)$/, (port, done) ->
    request-data =
      url: "http://localhost:#{port}/send/foo"
      method: 'POST'
      body:
        payload: ''
      json: yes
    request request-data, (err) ->
      expect(err).to.not.be.undefined
      expect(err.message).to.equal "connect ECONNREFUSED 127.0.0.1:#{port}"
      done!
