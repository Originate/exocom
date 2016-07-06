require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'livescript'
  'nitroglycerin' : N
  'portfinder'
  'wait' : {wait-until}
  'zmq'
}


module.exports = ->


  @Given /^an ExoRelay instance called "([^"]*)"$/, (instance-name) ->
    @exo-relay = new ExoRelay {@exocom-port, service-name: 'test'}
      ..on 'error', (@error) ~>


  @Given /^an ExoRelay instance called "([^"]*)" running inside the "([^"]*)" service at port (\d+)$/, (instance-name, service-name, port, done) ->
    @exocom-sender = zmq.socket 'push'
      ..connect "tcp://localhost:#{port}"
    @exo-relay = new ExoRelay {service-name, @exocom-port}
      ..on 'online', -> done!
      ..on 'error', (@error) ~>
      ..listen port


  @Given /^an ExoRelay instance called "([^"]*)" listening on port (\d+)$/, (instance-name, port, done) ->
    @exocom-sender = zmq.socket 'push'
      ..connect "tcp://localhost:#{port}"
    @exo-relay = new ExoRelay {@exocom-port, service-name: 'test'}
      ..on 'online', (@online-port) ~> done!
      ..on 'error', (@error) ~>
      ..listen port


  @Given /^an ExoRelay instance$/, ->
    @exo-relay = new ExoRelay {@exocom-port, service-name: 'test'}
      ..on 'online', (@online-port) ~>


  @Given /^an ExoRelay instance listening on port (\d+)$/, (port, done) ->
    @exocom-sender = zmq.socket 'push'
      ..connect "tcp://localhost:#{port}"
    @exo-relay = new ExoRelay exocom-port: @exocom-port, service-name: 'test'
      ..on 'online', -> done!
      ..on 'error', (@error) ~>
      ..listen port



  @When /^I create an ExoRelay instance .*: "([^"]*)"$/, (code) ->
    eval livescript.compile("@exo-relay = #{code}", bare: yes, header: no)



  @Then /^ExoRelay emits an "error" event with the error "([^"]*)"$/, (error-message) ->
    wait-until (~> @error), 1, ~>
      expect(@error.message).to.equal error-message
      @error = null


  @Then /^it emits the 'online' event with payload (\d+)$/ (+payload) ->
    expect(@online-port).to.equal payload


  @Then /^it throws the error "([^"]*)"$/, (expected-error) ->
    expect(@error).to.equal expected-error


  @Then /^my handler calls the "done" method$/, (done) ->
    wait-until (~> @done.called), 10, done
