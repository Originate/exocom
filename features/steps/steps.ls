require! {
  'chai' : {expect}
  '../support/dim-console'
  'exocomm-mock' : ExoCommMock
  'http'
  'livescript'
  'observable-process' : ObservableProcess
  'path'
  'record-http' : HttpRecorder
  'request'
  'wait' : {wait, wait-until}
}


module.exports = ->

  @Given /^an instance of the "([^"]*)" service$/, (@service-name, done) ->
    @exocomm.register-service name: @service-name, port: 4000
    @process = new ObservableProcess("bin/exo-js run --port 4000 --exocomm-port #{@exocomm-port}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port', done


  @Given /^this instance of the "([^"]*)" service:$/, (@service-name, code, done) ->
    @exocomm.register-service name: @service-name, port: 4000
    @process = new ObservableProcess("bin/#{code}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: yes)
      ..wait 'online at port', done


  @Given /^an instance of the "([^"]*)" service listening on port (\d+)$/, (@service-name, port, done) ->
    @exocomm.register-service name: @service-name, {port}
    @process = new ObservableProcess("bin/exo-js run --port #{port} --exocomm-port #{@exocomm-port}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', service-name),
                                     verbose: no)
      ..wait 'online at port 4000', done


  @Given /^I am in the "([^"]*)" service directory$/, (@service-name) ->


  @Given /^ExoComm is available at port (\d+)$/, (@exocomm-port, done) ->
    @exocomm = new ExoCommMock
      ..listen @exocomm-port, done


  @When /^executing "([^"]*)"$/, (command, done) ->
    @process = new ObservableProcess("bin/#{command}",
                                     cwd: path.join(process.cwd!, 'features', 'example-apps', @service-name),
                                     console: dim-console
                                     verbose: yes)
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


  @When /^receiving the( unknown)? "([^"]*)" command$/, (expect-error, command-name) ->
    @exocomm
      ..reset!
      ..send-command {service: @service-name, name: command-name, expect-error}


  @When /^receiving the( unknown)? "([^"]*)" command with the payload:$/, (expect-error, command-name, payload) ->
    eval livescript.compile "json-payload = {\n#{payload}\n}", bare: yes, header: no
    @exocomm
      ..reset!
      ..send-command {service: @service-name, name: command-name, payload: json-payload, expect-error}



  @Then /^it acknowledges the received command$/, (done) ->
    wait-until (~> @exocomm.last-send-response-code), ~>
      expect(@exocomm.last-send-response-code).to.equal 200
      done!


  @Then /^after a while it sends the "([^"]*)" command$/, (reply-command-name, done) ->
    @exocomm.wait-until-receive ~>
      received-commands = @exocomm.received-commands!
      expect(received-commands).to.have.length 1
      expect(received-commands[0].name).to.equal reply-command-name
      done!


  @Then /^after a while it sends the "([^"]*)" command with the textual payload:$/, (reply-command-name, payload-text, done) ->
    @exocomm.wait-until-receive ~>
      received-commands = @exocomm.received-commands!
      expect(received-commands).to.have.length 1
      expect(received-commands[0].name).to.equal reply-command-name
      expect(received-commands[0].payload).to.equal payload-text
      done!


  @Then /^it signals an unknown command$/, (done) ->
    wait-until (~> @exocomm.last-send-response-code), ~>
      expect(@exocomm.last-send-response-code).to.equal 404
      done!


  @Then /^its console output contains "([^"]*)"$/, (output, done) ->
    @process.wait output, done


  @Then /^(?:my service|it) returns a (\d+) response$/, (+expected-status, done) ->
    @exocomm.wait-until-receive ~>
      expect(@exocomm.calls[0].status-code).to.equal expected-status
      done!


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
