require! {
  'chai' : {expect}
  'request'
}


module.exports = ->

  @When /^I take it online at the default port$/, (done) ->
    @exo-relay
      ..on 'online', -> done!
      ..listen!


  @When /^I take it online at port (\d+)$/, (port, done) ->
    @exo-relay
      ..on 'online', -> done!
      ..listen port


  @When /^I try to take it online at port "([^"]*)"$/, (port, done) ->
    @exo-relay
      ..on 'error', (@error) ~> done!
      ..on 'online', -> done 'should not be online'
      ..listen port



  @Then /^it is online at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}/status", (err, response, body) ->
      expect(err).to.be.null
      expect(response.status-code).to.equal 200
      done!
