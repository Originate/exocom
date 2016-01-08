require! {
  '../..' : ExoRelay
  'chai' : {expect}
  '../example-apps/example-apps'
  'livescript'
  'request'
}


module.exports = ->


  @Given /^a running "([^"]*)" example application$/, (app-name, done) ->
    example-apps[app-name] done


  @When /^making a POST request to "([^"]*)"$/, (url, done) ->
    options =
      method: 'POST'
      url: url
      json: yes
    request options, (err, @response, body) ~>
      expect(err).to.be.falsy
      done!



  @Then /^it returns a (\d+) response$/, (expected-status) ->
    expect(@response.status-code).to.equal parse-int(expected-status, 10)
