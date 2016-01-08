require! {
  '../..' : ExoRelay
  'chai' : {expect}
  '../example-apps/example-apps'
  'livescript'
  'request'
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^an ExoRelay instance: "([^"]*)"$/, (code) ->
    eval "this.#{code}"


  @Given /^an ExoRelay instance listening at port (\d+)$/, (port, done) ->
    @exo-relay = new ExoRelay!
      ..listen port, done


  @Given /^I add a command listener:$/, (code) ->
    eval livescript.compile "@#{code}"



  @When /^I take it online at port (\d+): "([^"]*)"$/, (port, code, done) ->
    eval "this.#{code}"


  @When /^sending a POST request to "([^"]*)"$/, (url, done) ->
    options =
      method: 'POST'
      url: url
      json: yes
    request options, (err, @response, body) ~>
      expect(err).to.be.falsy
      done!



  @Then /^it is online at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}/run", (err, response, body) ->
      expect(err).to.be.falsy
      expect(response.status-code).to.equal 200
      done!


  @Then /^this command handler gets called$/, ->
    wait-until (~> @ran), 10
