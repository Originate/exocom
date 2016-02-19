require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'livescript'
  'nitroglycerin' : N
  'portfinder'
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^an ExoRelay instance called "([^"]*)"$/, (instance-name) ->
    @exo-relay = new ExoRelay {@exocomm-port}
      ..on 'error', (@error) ~>


  @Given /^an ExoRelay instance called "([^"]*)" running inside the "([^"]*)" service at port (\d+)$/, (instance-name, service-name, port, done) ->
    @exo-relay = new ExoRelay {service-name, @exocomm-port}
      ..on 'online', -> done!
      ..on 'error', (@error) ~>
      ..listen port


  @Given /^an ExoRelay instance called "([^"]*)" listening at port (\d+)$/, (instance-name, port, done) ->
    @exo-relay = new ExoRelay {@exocomm-port}
      ..on 'online', -> done!
      ..on 'error', (@error) ~>
      ..listen port


  @Given /^an ExoRelay instance$/, ->
    @exo-relay = new ExoRelay {@exocomm-port}


  @Given /^an ExoRelay instance listening at port (\d+)$/, (port, done) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port
      ..on 'online', -> done!
      ..on 'error', (@error) ~>
      ..listen port



  @When /^I create an ExoRelay instance .*: "([^"]*)"$/, (code) ->
    eval livescript.compile("@exo-relay = #{code}", bare: yes, header: no)



  @Then /^ExoRelay emits an "error" event with the message "([^"]*)"$/, (message) ->
    expect(@error).to.not.be.null
    expect(@error.message).to.equal message
    @error = null


  @Then /^it throws the error "([^"]*)"$/, (expected-error) ->
    expect(@error).to.equal expected-error


  @Then /^my handler calls the "done" method$/, (done) ->
    wait-until (~> @done.called), 10, done
