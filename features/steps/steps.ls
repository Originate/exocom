require! {
  'chai' : {expect}
  '../support/dim-console'
  'exocomm-mock' : ExoCommMock
  'http'
  'livescript'
  'observable-process' : ObservableProcess
  'record-http' : HttpRecorder
  'request'
  'wait' : {wait, wait-until}
}


module.exports = ->

  @Given /^an ExoComm instance$/, (done) ->
    @exocomm-port = 4100
    @exocomm = new ExoCommMock
      ..listen @exocomm-port, done


  @Given /^an instance of the "([^"]*)" service$/, (@service-name, done) ->
    @exocomm.register-service name: @service-name, port: 4000
    @create-exoservice-instance {@service-name, port: 4000, @exocomm-port}, done


  @Given /^ports (\d+) and (\d+) are used$/, (port1, port2, done) ->
    # Note: this is due to a Cucumber-JS issue where cleanup methods aren't async.
    # So we have to let all remaining commands in the event queue be processed here
    # so that any code that releases ports has actually been executed.
    wait 100, ~>
      handler = (_, res) -> res.end 'existing server'
      @server1 = http.create-server(handler).listen 3000, 'localhost', ~>
        @server2 = http.create-server(handler).listen 3001, 'localhost', done



  @When /^receiving the( unknown)? "([^"]*)" command$/, (expect-error, command-name) ->
    @exocomm
      ..reset!
      ..send-command {service: @service-name, name: command-name, expect-error}


  @When /^receiving the( unknown)? "([^"]*)" command with the payload:$/, (expect-error, command-name, payload) ->
    eval livescript.compile "json-payload = {\n#{payload}\n}", bare: yes, header: no
    @exocomm
      ..reset!
      ..send-command {service: @service-name, name: command-name, payload: json-payload, expect-error}


  @When /^starting a service$/, (done) ->
    @service-name = 'test'
    @exocomm.register-service name: @service-name, port: 3000
    @create-exoservice-instance {@service-name, @exocomm-port}, done


  @When /^starting a service at port (\d+)$/, (port, done) ->
    @service-name = 'test'
    @exocomm.register-service {name: @service-name, port}
    @create-exoservice-instance {@service-name, port, @exocomm-port}, done


  @When /^starting the "([^"]*)" service$/, (@service-name, done) ->
    @exocomm.register-service name: @service-name, port: 3000
    @create-exoservice-instance {@service-name, @exocomm-port}, done



  @Then /^it acknowledges the received command$/, (done) ->
    wait-until (~> @exocomm.last-send-response-code), ~>
      expect(@exocomm.last-send-response-code).to.equal 200
      done!


  @Then /^it can run the "([^"]*)" service$/, (@service-name, done) ->
    @create-exoservice-instance {@service-name, port: 4000, @exocomm-port}, done


  @Then /^it runs the "([^"]*)" hook$/, (hook-name, done) ->
    @exocomm
      ..reset!
      ..send-command name: 'which-hooks-ran', service: @service-name
      ..wait-until-receive ~>
        expect(@exocomm.received-commands![0].payload).to.eql ['before-all']
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


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
