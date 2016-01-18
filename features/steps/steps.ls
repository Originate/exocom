require! {
  'chai' : {expect}
  'http'
  'observable-process' : ObservableProcess
  'request'
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^another service already uses port (\d+)$/, (port, done) ->
    handler = (_, res) -> res.end 'existing server'
    @existing-server = http.create-server(handler).listen port, ->
      request "http://localhost:3100", (err, _, body) ->
        expect(err).to.be.null
        expect(body).to.equal 'existing server'
        done!



  @When /^I run "([^"]*)"$/, (code) ->
    @process = new ObservableProcess code, verbose: no



  @Then /^it aborts$/, (done) ->
    wait-until (~> @process.crashed), done


  @Then /^this service runs at port (\d+)$/, (port, done) ->
    @process.wait "online at port #{port}", done
