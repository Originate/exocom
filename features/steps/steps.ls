require! {
  'chai' : {expect}
  'http'
  'livescript'
  'observable-process' : ObservableProcess
  'path'
  'record-http' : HttpRecorder
  'request'
  'wait' : {wait, wait-until}
}


module.exports = ->

  @Given /^an instance of the "([^"]*)" service$/, (service-name, done) ->
    @process = new ObservableProcess("bin/exo-js run --port 4000 --exocomm-port #{@exocomm-port}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port', done


  @Given /^this instance of the "([^"]*)" service:$/, (service-name, code, done) ->
    @process = new ObservableProcess("bin/#{code}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port', done


  @Given /^an instance of the "([^"]*)" service listening on port (\d+)$/, (service-name, port, done) ->
    @process = new ObservableProcess("bin/exo-js run --port #{port} --exocomm-port #{@exocomm-port}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port 4000', done


  @Given /^I am in the "([^"]*)" service directory$/, (@service-name) ->


  @Given /^ExoComm is available at port (\d+)$/, (@exocomm-port, done) ->
    @exocomm = new HttpRecorder().listen @exocomm-port, done


  @When /^executing "([^"]*)"$/, (command, done) ->
    @process = new ObservableProcess("bin/#{command}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', @service-name),
                                     verbose: no)
      ..wait 'online at port', done


  @Given /^ports (\d+) and (\d+) are used$/, (port1, port2, done) ->
    # Note: this is due to a Cucumber-JS issue where cleanup methods aren't async.
    # So we have to let all remaining commands in the event queue be processed here
    # so that any code that releases ports has actually been executed.
    process.next-tick ~>
      wait 100, ~>
        handler = (_, res) -> res.end 'existing server'
        @server1 = http.create-server(handler).listen 3000, 'localhost', ~>
          @server2 = http.create-server(handler).listen 3001, 'localhost', done



  @When /^sending a POST request to "([^"]*)"$/, (path, done) ->
    request.post url: "http://localhost:4000#{path}", (err, @response, body) ~>
      expect(err).to.be.falsy
      done!


  @When /^receiving the( unknown)? "([^"]*)" command$/, (expect-error, command-name, done) ->
    data =
      url: "http://localhost:4000/run/#{command-name}",
      method: 'POST'
      body:
        requestId: '123'
      json: yes
    request data, (err, @response, body) ~>
      expect(err).to.be.null
      expect(@response.status-code).to.equal 200 unless expect-error
      done!


  @When /^receiving the(?: unknown)? "([^"]*)" command with the payload:$/, (command-name, payload, done) ->
    data =
      url: "http://localhost:4000/run/#{command-name}",
      method: 'POST'
      body:
        requestId: '123'
        payload: JSON.parse(payload)
      json: yes
    request data, (err, @response, body) ~>
      expect(err).to.be.null
      expect(response.status-code).to.equal 200
      done!



  @Then /^(?:my service|it) returns a (\d+) response$/, (+expected-status) ->
    expect(@response.status-code).to.equal expected-status


  @Then /^it sends the "([^"]*)" command$/, (reply-command-name, done) ->
    wait-until (~> @exocomm.calls.length), ~>
      call = @exocomm.calls[0]
      expect(call.url).to.equal "http://localhost:#{@exocomm-port}/send/pong"
      done!


  @Then /^it sends the "([^"]*)" command with the payload "([^"]*)"$/, (reply-command-name, expected-payload, done) ->
    wait-until (~> @exocomm.calls.length), ~>
      wait 100, ~>
        call = @exocomm.calls[0]
        expect(call.url).to.equal "http://localhost:#{@exocomm-port}/send/#{reply-command-name}"
        expect(call.body.payload).to.equal expected-payload
        done!


  @Then /^its console output contains "([^"]*)"$/, (output, done) ->
    @process.wait output, done


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
