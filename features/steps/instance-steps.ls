require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'livescript'
  'nitroglycerin' : N
  'portfinder' : {get-port}
  'record-http' : HttpRecorder
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^an ExoRelay instance called "([^"]*)"$/, (instance-name) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port
      ..on 'error', (@error) ~>


  @Given /^an ExoRelay instance called "([^"]*)" listening at port (\d+)$/, (instance-name, port, done) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port
      ..on 'error', (@error) ~>
      ..listen port, done


  @Given /^an ExoRelay instance: "([^"]*)"$/, (code) ->
    eval "this.#{code}"
    @exo-relay.on 'error', (@error) ~>


  @Given /^an ExoRelay instance listening at port (\d+)$/, (port, done) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port
      ..on 'error', (@error) ~>
      ..listen port, done


  @Given /^ExoComm runs at port (\d+)$/, (@exocomm-port, done) ->
    @exocomm = new HttpRecorder!listen @exocomm-port, done



  @When /^I create an ExoRelay instance .*: "([^"]*)"$/, (code) ->
    eval livescript.compile("@exo-relay = #{code}", bare: yes, header: no)



  @Then /^ExoRelay emits an "error" event with the message "([^"]*)"$/, (message) ->
    expect(@error).to.not.be.null
    expect(@error.message).to.equal message
    @error = null


  @Then /^my handler calls the "done" method$/, (done) ->
    wait-until (~> @done.called), 10, done


  @Then /^this instance uses the ExoComm port (\d+)$/, (+port) ->
    expect(@exo-relay.command-sender.exocomm-port).to.equal port
