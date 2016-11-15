require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'exocom-mock': MockExoCom
  'livescript'
  'nitroglycerin' : N
  'portfinder'
  'prelude-ls' : {any}
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^an ExoRelay instance called "([^"]*)"$/, (instance-name, done) ->
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: @service-name = \test-service}
      ..connect!
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          if @exocom.received-messages |> any (.name is "exocom.register-service")
            @exocom.reset!
          done!
      ..on 'error', (@error) ~>


  @Given /^an ExoRelay instance called "([^"]*)" running inside the "([^"]*)" service$/, (instance-name, @service-name, done) ->
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @service-name, @exocom-port}
      ..connect!
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          if @exocom.received-messages |> any (.name is "exocom.register-service")
            @exocom.reset!
          done!
      ..on 'error', (@error) ~>


  @Given /^an ExoRelay instance$/, (done) ->
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: @service-name = \test-service}
      ..connect!
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          if @exocom.received-messages |> any (.name is "exocom.register-service")
            @exocom.reset!
          done!
      ..on 'error', (@error) ~>


  @Given /^a new ExoRelay instance connecting to port (\d+)$/ (@exocom-port) ->
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: @service-name = 'test-service'}


  @When /^an ExoRelay instance running inside the "([^"]*)" service comes online$/ (@service-name, done) ->
    @exo-relay = new ExoRelay {@service-name, @exocom-port, exocom-host: "localhost"}
      ..connect!
      ..on 'online', ~>
        @message-id = @exo-relay.websocket-connector.last-sent-id
        done!
      ..on 'error', (@error) ~>


  @When /^I take it online$/, (done) ->
    @exo-relay
      ..connect!
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          if @exocom.received-messages |> any (.name is "exocom.register-service")
            @exocom.reset!
          done!


  @When /^I try to take it online$/, (done) ->
    @exo-relay
      ..connect!
        ..on 'error', (@error) ~>
          done!


  @Then /^it connects to the given ExoCom host and port$/, (done) ->
    @exocom
      ..send service: @service-name, name: '__status'
    current-length = @exocom.received-messages.length
    wait-until (~> @exocom.received-messages.length > current-length), 1, ~>
      if @exocom.received-messages |> any (.name is "__status-ok")
        done!


  @When /^I create an ExoRelay instance .*: "([^"]*)"$/, (code) ->
    eval livescript.compile("@exo-relay = #{code}", bare: yes, header: no)


  @Then /^ExoRelay emits an "error" event with the error "([^"]*)"$/, (error-message, done) ->
    wait-until (~> @error), 1, ~>
      expect(@error.message).to.equal error-message
      @error = null
      done!


  @Then /^it throws the error "([^"]*)"$/, (expected-error) ->
    expect(@error).to.equal expected-error


  @Then /^my handler calls the "done" method$/, (done) ->
    wait-until (~> @done.called), 10, done
