require! {
  'chai' : {expect}
  '../..' : MockExoComm
  'record-http' : HttpRecorder
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^a ExoCommMock instance$/, ->
    @exocomm = new MockExoComm


  @Given /^a known "([^"]*)" service listening at port (\d+)$/, (service-name, port, done) ->
    @exocomm.register-service service-name, port
    @service = new HttpRecorder().listen port, done



  @When /^trying to send a "([^"]*)" command to the "([^"]*)" service$/, (command, service, done) ->
    @exocomm.send {service, command}, (@error) ~>
      done!


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


  @Then /^I get the error "([^"]*)"$/, (expected-error) ->
    expect(@error.message).to.equal expected-error
