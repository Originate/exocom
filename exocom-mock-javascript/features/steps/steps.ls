require! {
  '../..' : MockExoCom
  'async'
  'chai'
  'jsdiff-console'
  'lowercase-keys'
  'nitroglycerin': N
  'port-reservation'
  'prelude-ls' : {filter, map, pairs-to-obj}
  'record-http' : HttpRecorder
  'request'
  'sinon'
  'sinon-chai'
  '../support/mock-exorelay' : MockExorelay
  'wait' : {wait, wait-until}
}
expect = chai.expect
chai.use sinon-chai


module.exports = ->

  @Given /^an ExoComMock instance$/, (done) ->
    @exocom = new MockExoCom
    port-reservation.get-port N (@exocom-port) ~>
      @exocom.listen @exocom-port, done


  @Given /^a known "([^"]*)" service$/, (name, done) ->
    @create-named-websocket-endpoint {name, @exocom-port}, done


  @Given /^somebody sends it a message$/, (done) ->
    @create-websocket-endpoint {@exocom-port}, ~>
      old-length = @exocom.received-messages.length
      @service-send-message name: \foo, payload: '', id: \123
      wait-until (~> @exocom.received-messages.length > old-length), 1, done


  @Given /^somebody sends it a "([^"]*)" message with payload "([^"]*)"$/, (name, payload, done) ->
    @create-websocket-endpoint {@exocom-port}, ~>
      old-length = @exocom.received-messages.length
      @service-send-message name: name, payload: payload, id: \123
      wait-until (~> @exocom.received-messages.length > old-length), 1, done


  @When /^closing it$/, (done) ->
    @exocom.close done


  @When /^I tell it to wait for a call$/, ->
    @call-received = sinon.spy!
    @exocom.on-receive @call-received


  @When /^a call comes in$/, (done) ->
    @create-websocket-endpoint {@exocom-port}, ~>
      @service-send-message {name: \foo, id: \123}
      done!


  @When /^a new service instance registers itself with it via the message:$/ (table, done) ->
    table-data = table.raw! |> pairs-to-obj
    payload = table-data.PAYLOAD |> JSON.parse
    @create-named-websocket-endpoint {name: 'test instance', @exocom-port, registration-message: table-data.NAME, registration-payload: payload}, done


  @When /^trying to send a "([^"]*)" message to the "([^"]*)" service$/, (message-name, service-name, done) ->
    try
      @exocom.send service: service-name, name: message-name
    catch
      @error = e
      done!


  @When /^resetting the ExoComMock instance$/, ->
    @exocom.reset!


  @When /^sending a "([^"]*)" message to the "([^"]*)" service with the payload:$/, (message, service, payload) ->
    @exocom-send-message {@exocom, service, message-data: {name: message, payload: payload}}



  @Then /^ExoCom now knows about these services:$/ (table) ->
    async.each table.raw!,
               (~> @exocom.wait-until-knows-service &1, &2)


  @Then /^ExoComMock makes the request:$/, (table) ->
    @verify-exocom-received-request table.rows-hash!


  @Then /^I can close it without errors$/, (done) ->
    @exocom.close done


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
      expected-messages = table.hashes! |> map lowercase-keys
      service-messages = filter (.name is not "exocom.register-service"), @exocom.received-messages
      jsdiff-console service-messages, expected-messages, done


  @Then /^it is no longer listening$/, (done) ->
    request-data =
      url: "http://localhost:#{@exocom-port}/send/foo"
      method: 'POST'
      body:
        payload: ''
      json: yes
    request request-data, (err) ~>
      expect(err).to.not.be.undefined
      expect(err.message).to.equal "connect ECONNREFUSED 127.0.0.1:#{@exocom-port}"
      done!
