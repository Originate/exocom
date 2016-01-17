require! {
  'chai' : {expect}
  'livescript'
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


  @Given /^ports (\d+) and (\d+) are used$/, (port1, port2, done) ->
    handler = (_, res) -> res.end 'existing server'
    @server1 = http.create-server(handler).listen 3000, 'localhost', ~>
      @server2 = http.create-server(handler).listen 3001, 'localhost', done



  @When /^sending a POST request to "([^"]*)"$/, (path, done) ->
    request.post url: "http://localhost:4000#{path}", (err, @response, body) ~>
      expect(err).to.be.falsy
      done!


  @When /^sending the request:$/, (request-data, done) ->
    eval livescript.compile "compiled = {\n#{request-data}\n}", bare: yes, header: no
    compiled.json = yes
    request compiled, (err, @response, body) ~>
      expect(err).to.be.null
      done!



  @Then /^it returns a (\d+) response$/, (expected-status) ->
    expect(@response.status-code).to.equal parse-int(expected-status, 10)


  @Then /^its console output contains "([^"]*)"$/, (output, done) ->
    @process.wait output, done


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
