require! {
  'chai' : {expect}
  'request'
}


module.exports = ->

  @When /^I check the status$/, (done) ->
    request "http://localhost:#{@exo-relay.port}/status", (err, response) ~>
      expect(err).to.be.null
      @status-code = response.status-code
      done!



  @Then /^it signals it is online$/, ->
    expect(@status-code).to.equal 200
