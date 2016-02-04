require! {
  '../..' : MockExoComm
  'chai' : {expect}
  'jsdiff-console'
  'record-http' : HttpRecorder
  'request'
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^an ExoCommMock instance$/, ->
    @exocomm = new MockExoComm


  @Given /^an ExoCommMock instance listening at port (\d+)$/, (port, done) ->
    @exocomm = new MockExoComm
      ..listen port, done


  @Given /^a known "([^"]*)" service listening at port (\d+)$/, (service-name, port, done) ->
    @exocomm.register-service service-name, port
    @service = new HttpRecorder().listen port, done


  @Given /^somebody sends it a command$/, (done) ->
    request-data =
      url: "http://localhost:#{@exocomm.port}/send/foo"
      method: "POST"
      body:
        payload: ''
        request-id: '123'
      json: yes
    request request-data, done


  @Given /^somebody sends it a "([^"]*)" command with payload "([^"]*)"$/, (command, payload, done) ->
    request-data =
      url: "http://localhost:#{@exocomm.port}/send/#{command}"
      method: "POST"
      body:
        payload: payload
        request-id: '123'
      json: yes
    request request-data, done



  @When /^closing it$/, ->
    @exocomm.close!


  @When /^trying to send a "([^"]*)" command to the "([^"]*)" service$/, (command, service, done) ->
    @exocomm.send {service, command}, (@error) ~>
      done!


  @When /^resetting the ExoCommMock instance$/, ->
    @exocomm.reset!


  @When /^sending a "([^"]*)" command to the "([^"]*)" service with the payload:$/, (command, service, payload, done) ->
    @exocomm.send {service, command, payload}, done



  @Then /^ExoCommMock makes the request:$/, (table, done) ->
    expected-request = table.rows-hash!
    wait-until (~> @service.calls.length is 1), 10, ~>
      actual-request = @service.calls[0]
      expect(actual-request.url).to.equal expected-request.URL
      expect(actual-request.method).to.equal expected-request.METHOD
      expect(actual-request.body.payload).to.equal expected-request.PAYLOAD
      done!


  @Then /^I can close it without errors$/, ->
    @exocomm.close!


  @Then /^I get the error "([^"]*)"$/, (expected-error) ->
    expect(@error.message).to.equal expected-error


  @Then /^it has received no commands/, ->
    expect(@exocomm.calls).to.be.empty


  @Then /^it has received the commands/, (table, done) ->
    expected-commands = [{[key.to-lower-case!, value] for key, value of command} for command in table.hashes!]
    actual-commands = @exocomm.received-commands!
    jsdiff-console actual-commands, expected-commands, done


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
