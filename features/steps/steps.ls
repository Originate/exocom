require! {
  'chai' : {expect}
  '../../lib/observable-process' : ObservableProcess
  'path'
  'request'
  'wait'
}


module.exports = ->

  @Given /^a running instance of the "([^"]*)" service$/, (service-name, done) ->
    @process = new ObservableProcess("bin/exoservice-js run",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name))
      ..wait 'online at port', done


  @Given /^I am in the "([^"]*)" service directory$/, (@service-name) ->



  @When /^executing "([^"]*)"$/, (command, done) ->
    @process = new ObservableProcess("bin/#{command}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', @service-name))
      ..wait 'online at port', done


  @When /^I send it a POST request to "([^"]*)"$/, (path, done) ->
    request.post url: "http://localhost:3000/#{path}", (err, response, body) ->
      expect(err).to.be.falsy
      expect(response.status-code).to.equal 200
      done!



  @Then /^its console output contains "([^"]*)"$/, (output, done) ->
    @process.wait output, done


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", -> done!
