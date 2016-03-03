require! {
  '../..' : MockExoCom
  'chai'
  'jsdiff-console'
  'record-http' : HttpRecorder
  'request'
  'sinon'
  'sinon-chai'
  'wait' : {wait-until}
}
expect = chai.expect
chai.use sinon-chai


module.exports = ->

  @Given /^a listening ExoComMock instance$/, (done) ->
    @exocom = new MockExoCom
      ..listen 4111, done


  @Given /^an ExoComMock instance$/, ->
    @exocom = new MockExoCom


  @Given /^an ExoComMock instance listening at port (\d+)$/, (port, done) ->
    @exocom = new MockExoCom
      ..listen port, done


  @Given /^a known "([^"]*)" service listening at port (\d+)$/, (name, port, done) ->
    @exocom.register-service {name, port}
    @service = new HttpRecorder().listen port, done


  @Given /^somebody sends it a message$/, (done) ->
    request-data =
      url: "http://localhost:#{@exocom.port}/send/foo"
      method: "POST"
      body:
        payload: ''
        request-id: '123'
      json: yes
    request request-data, done


  @Given /^somebody sends it a "([^"]*)" message with payload "([^"]*)"$/, (message, payload, done) ->
    request-data =
      url: "http://localhost:#{@exocom.port}/send/#{message}"
      method: "POST"
      body:
        payload: payload
        request-id: '123'
      json: yes
    request request-data, done



  @When /^closing it$/, ->
    @exocom.close!


  @When /^I tell it to wait for a call$/, ->
    @call-received = sinon.spy!
    @exocom.wait-until-receive @call-received


  @When /^a call comes in$/, (done) ->
    request-data =
      url: "http://localhost:#{@exocom.port}/send/foo"
      method: "POST"
      json: yes
    request request-data, done


  @When /^trying to send a "([^"]*)" message to the "([^"]*)" service$/, (message-name, service-name) ->
    try
      @exocom.send-message service: service-name, name: message-name, (@error)
    catch
      @error = e


  @When /^resetting the ExoComMock instance$/, ->
    @exocom.reset!


  @When /^sending a "([^"]*)" message to the "([^"]*)" service with the payload:$/, (message, service, payload) ->
    @exocom.send-message service: service, name: message, payload: payload



  @Then /^ExoComMock lists the last send response code as (\d+)$/, (+expected-response-code, done) ->
    wait-until (~> @exocom.last-send-response-code), ~>
      expect(@exocom.last-send-response-code).to.equal expected-response-code
      done!


  @Then /^ExoComMock makes the request:$/, (table, done) ->
    expected-request = table.rows-hash!
    wait-until (~> @service.calls.length is 1), 10, ~>
      actual-request = @service.calls[0]
      expect(actual-request.url).to.equal expected-request.URL
      expect(actual-request.method).to.equal expected-request.METHOD
      expect(actual-request.body.payload).to.equal expected-request.PAYLOAD
      done!


  @Then /^I can close it without errors$/, ->
    @exocom.close!


  @Then /^I get the error "([^"]*)"$/, (expected-error) ->
    expect(@error.message).to.equal expected-error


  @Then /^it calls the given callback$/, (done) ->
    wait-until (~> @call-received.called), done


  @Then /^it calls the given callback right away$/, ->
    expect(@call-received).to.have.been.called


  @Then /^it doesn't call the given callback right away$/, ->
    expect(@call-received).to.not.have.been.called


  @Then /^it has received no messages/, ->
    expect(@exocom.calls).to.be.empty


  @Then /^it has received the messages/, (table, done) ->
    expected-messages = [{[key.to-lower-case!, value] for key, value of message} for message in table.hashes!]
    actual-messages = @exocom.received-messages!
    jsdiff-console actual-messages, expected-messages, done


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
