require! {
  'chai' : {expect}
  'observable-process' : ObservableProcess
  'path'
  'request'
}


module.exports = ->

  @Given /^an instance of the "([^"]*)" service listening on port (\d+)$/, (service-name, port, done) ->
    @process = new ObservableProcess("bin/exo-js run --port #{port}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port 4000', done


  @Given /^I am in the "([^"]*)" service directory$/, (@service-name) ->



  @When /^executing "([^"]*)"$/, (command, done) ->
    @process = new ObservableProcess("bin/#{command}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', @service-name),
                                     verbose: no)
      ..wait 'online at port', done


  @When /^sending a POST request to "([^"]*)"$/, (path, done) ->
    request.post url: "http://localhost:4000#{path}", (err, @response, body) ~>
      expect(err).to.be.falsy
      done!


  @When /^sending the request:$/, (request-data, done) ->
    options = JSON.parse request-data
    options.json = yes
    request options, (err, @response, body) ~>
      expect(err).to.be.null
      done!



  @Then /^it returns a (\d+) response$/, (expected-status) ->
    expect(@response.status-code).to.equal parse-int(expected-status, 10)


  @Then /^its console output contains "([^"]*)"$/, (output, done) ->
    @process.wait output, done


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", -> done!
