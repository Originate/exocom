require! {
  'chai' : {expect}
  'record-http' : HttpRecorder
  'request'
}


module.exports = ->

  @Given /^ExoComm runs at port (\d+)$/, (@exocomm-port, done) ->
    @exo-messaging = new HttpRecorder!listen @exocomm-port, done



  @When /^I take it online at port (\d+): "([^"]*)"$/, (port, code, done) ->
    eval "this.#{code}"


  @When /^I take it online at the default port: "([^"]*)"$/, (code, done) ->
    eval "this.#{code}"



  @Then /^it is online at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}/run", (err, response, body) ->
      expect(err).to.be.falsy
      expect(response.status-code).to.equal 200
      done!


