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


  @Given /^an ExoRelay instance called "([^"]*)"$/, (instance-name) ->
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: 'test-service'}
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          @exocom.reset! if @exocom.received-messages |> any (.name is 'exocom.register-service')
      ..on 'error', (@error) ~>


  @Given /^an ExoRelay instance called "([^"]*)" running inside the "([^"]*)" service at port (\d+)$/, (instance-name, service-name, port, done) ->
    @exocom.register-service name: service-name, port: port
    @exo-relay = new ExoRelay {exocom-host: 'localhost', service-name, @exocom-port}
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          @exocom.reset! if @exocom.received-messages |> any (.name is 'exocom.register-service')
          done!
      ..on 'error', (@error) ~>
      ..listen port


  @Given /^an ExoRelay instance called "([^"]*)" listening on port (\d+)$/, (instance-name, port, done) ->
    @exocom.register-service name: 'test-service', port: port
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: 'test-service'}
      ..on 'online', (@online-port) ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          @exocom.reset! if @exocom.received-messages |> any (.name is 'exocom.register-service')
          done!
      ..on 'error', (@error) ~>
      ..listen port


  @Given /^an ExoRelay instance$/, ->
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: 'test-service'}
      ..on 'online', (@online-port) ~>


  @Given /^an ExoRelay instance listening on port (\d+)$/, (port, done) ->
    @exocom.register-service name: 'test-service', port: port
    @exo-relay = new ExoRelay {exocom-host: 'localhost', @exocom-port, service-name: 'test-service'}
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          @exocom.reset! if @exocom.received-messages |> any (.name is 'exocom.register-service')
          done!
      ..on 'error', (@error) ~>
      ..listen port


  @When /^an ExoRelay instance running inside the "([^"]*)" service comes online at port (\d+)$/ (service-name, port, done) ->
    @exo-relay = new ExoRelay {service-name, @exocom-port, exocom-host: "localhost"}
      ..on 'online', ~>
        @message-id = @exo-relay.message-sender.last-sent-id
        done!
      ..on 'error', (@error) ~>
      ..listen port


  @When /^I create an ExoRelay instance .*: "([^"]*)"$/, (code) ->
    eval livescript.compile("@exo-relay = #{code}", bare: yes, header: no)


  @Then /^ExoRelay emits an "error" event with the error "([^"]*)"$/, (error-message, done) ->
    wait-until (~> @error), 1, ~>
      expect(@error.message).to.equal error-message
      @error = null
      done!


  @Then /^it emits the 'online' event with payload (\d+)$/ (+payload) ->
    expect(@online-port).to.equal payload


  @Then /^it throws the error "([^"]*)"$/, (expected-error) ->
    expect(@error).to.equal expected-error


  @Then /^my handler calls the "done" method$/, (done) ->
    wait-until (~> @done.called), 10, done
