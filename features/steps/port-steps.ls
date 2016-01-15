require! {
  'chai' : {expect}
  'request'
}


module.exports = ->

  @When /^I( try to)? take it online at.*: "([^"]*)"$/, (expect-error, code, done) ->
    try
      eval "this.#{code}"
      return done 'Expected error' if expect-error
    catch
      @error = e.message
      done!


  @Then /^it is online at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}/run", (err, response, body) ->
      expect(err).to.be.null
      expect(response.status-code).to.equal 200
      done!


