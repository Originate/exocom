require! {
  'chai' : {expect}
  'dim-console'
  'exocom-mock' : ExoComMock
  'http'
  'livescript'
  'nitroglycerin': N
  'port-reservation'
  'prelude-ls' : {any}
  'record-http' : HttpRecorder
  'wait' : {wait, wait-until}
}


module.exports = ->

  @Given /^an ExoCom instance$/, (done) ->
    port-reservation.get-port N (@exocom-port) ~>
      @exocom = new ExoComMock
        ..listen @exocom-port
      done!

  @Given /^an ExoCom instance running at port (\d+)$/, (@exocom-port) ->
    @exocom = new ExoComMock
      ..listen @exocom-port


  @Given /^an instance of the "([^"]*)" service$/, (@service-name, done) ->
    @create-exoservice-instance {@service-name, @exocom-port}, ~>
      @remove-register-service-message @exocom, done


  @Given /^ports (\d+) and (\d+) are used$/, (port1, port2, done) ->
    # Note: this is due to a Cucumber-JS issue where cleanup methods aren't async.
    # So we have to let all remaining messages in the event queue be processed here
    # so that any code that releases ports has actually been executed.
    wait 100, ~>
      handler = (_, res) -> res.end 'existing server'
      @server1 = http.create-server(handler).listen 3000, 'localhost', ~>
        @server2 = http.create-server(handler).listen 3001, 'localhost', done



  @When /^receiving the( unknown)? "([^"]*)" message$/, (expect-error, message-name) ->
    @exocom
      ..reset!
      ..send {service: @service-name, name: message-name, expect-error}


  @When /^receiving the( unknown)? "([^"]*)" message with the payload:$/, (expect-error, message-name, payload) ->
    eval livescript.compile "json-payload = {\n#{payload}\n}", bare: yes, header: no
    @exocom
      ..reset!
      ..send {service: @service-name, name: message-name, payload: json-payload, expect-error}


  @When /^starting a service$/, (done) ->
    @service-name = 'test service'
    @create-exoservice-instance {@service-name, @exocom-port}, ~>
      @remove-register-service-message @exocom, done


  @When /^starting a service configured for ExoCom port (\d+)$/, (port, done) ->
    @service-name = 'test service'
    @create-exoservice-instance {@service-name, exocom-port: port}, ~>
      @remove-register-service-message @exocom, done


  @When /^starting the "([^"]*)" service$/, (@service-name, done) ->
    @create-exoservice-instance {@service-name, @exocom-port}, ~>
      @remove-register-service-message @exocom, done


  @When /^trying to start a service configured for ExoCom port (\d+)$/, (port) ->
    @service-name = 'test service'
    @create-exoservice-instance {@service-name, exocom-port: port}


  @Then /^after a while it sends the "([^"]*)" message$/, (reply-message-name, done) ->
    @exocom.on-receive ~>
      received-messages = @exocom.received-messages
      expect(received-messages).to.have.length 1
      expect(received-messages[0].name).to.equal reply-message-name
      done!


  @Then /^after a while it sends the "([^"]*)" message with the textual payload:$/, (reply-message-name, payload-text, done) ->
    @exocom.on-receive ~>
      received-messages = @exocom.received-messages
      expect(received-messages).to.have.length 1
      expect(received-messages[0].name).to.equal reply-message-name
      expect(received-messages[0].payload).to.equal payload-text
      done!


  @Then /^it acknowledges the received message$/, (done) ->
    wait-until (~> @exocom.received-messages.length), done


  @Then /^it can run the "([^"]*)" service$/, (@service-name, done) ->
    @create-exoservice-instance {@service-name, @exocom-port}, done


  @Then /^it runs the "([^"]*)" hook$/, (hook-name, done) ->
    @exocom
      ..reset!
      ..send name: 'which-hooks-ran', service: @service-name
      ..on-receive ~>
        expect(@exocom.received-messages[0].payload).to.eql ['before-all']
        done!


  @Then /^it connects to the ExoCom instance$/, (done) ->
    @exocom.send service: @service-name, name: '__status' , id: '123'
    wait-until (~> @exocom.received-messages[0]), 1, ~>
      if @exocom.received-messages[0].name is "__status-ok"
        done!

  @Then /^it aborts with the error message "([^"]*)"$/ (error-message, done) ->
    #TODO: implement this
    done!
