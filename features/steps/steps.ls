require! {
  '../..' : MockExoCom
  'chai'
  'jsdiff-console'
  'nitroglycerin': N
  'port-reservation'
  'prelude-ls' : {filter}
  'record-http' : HttpRecorder
  'request'
  'sinon'
  'sinon-chai'
  '../support/websocket-endpoint' : WebSocketEndpoint
  'wait' : {wait, wait-until}
}
expect = chai.expect
chai.use sinon-chai


module.exports = ->

  @Given /^a listening ExoComMock instance$/, (done) ->
    @exocom = new MockExoCom
    port-reservation.get-port N (@exocom-port) ~>
      @exocom.listen @exocom-port
      done!


  @Given /^an ExoComMock instance$/, ->
    @exocom = new MockExoCom


  @Given /^an ExoComMock instance listening at port (\d+)$/, (@exocom-port) ->
    @exocom = new MockExoCom
      ..listen @exocom-port


  @Given /^a known "([^"]*)" service$/, (name) ->
    @create-named-websocket-endpoint {name, port: @exocom-port}


  @Given /^somebody sends it a message$/, (done) ->
    @create-websocket-endpoint @exocom-port
    @service-send-message {name: \foo, payload: '', id: \123}, done


  @Given /^somebody sends it a "([^"]*)" message with payload "([^"]*)"$/, (name, payload, done) ->
    @create-websocket-endpoint @exocom-port
    @service-send-message {name: name, payload: payload, id: \123}, done


  @When /^closing it$/, ->
    @exocom.close!


  @When /^I tell it to wait for a call$/, ->
    @call-received = sinon.spy!
    @exocom.on-receive @call-received


  @When /^a call comes in$/, (done) ->
    @create-websocket-endpoint @exocom-port
    @service-send-message {name: \foo, id: \123}, done


  @When /^trying to send a "([^"]*)" message to the "([^"]*)" service$/, (message-name, service-name, done) ->
    try
      @exocom.send service: service-name, name: message-name
    catch
      @error = e
      done!


  @When /^resetting the ExoComMock instance$/, ->
    @exocom.reset!


  @When /^sending a "([^"]*)" message to the "([^"]*)" service with the payload:$/, (message, service, payload, done) ->
    @exocom-send-message {@exocom, service, message-data: {name: message, payload: payload}}, done



  @Then /^ExoComMock makes the request:$/, (table) ->
    @verify-exocom-received-request table.rows-hash!


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
    wait-until (~> @exocom.received-messages.length > 1), 10, ~>
      expected-messages = [{[key.to-lower-case!, value] for key, value of message} for message in table.hashes!]
      service-messages = filter (.name is not "exocom.register-service"), @exocom.received-messages
      jsdiff-console service-messages, expected-messages, done


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
