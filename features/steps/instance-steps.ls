require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'nitroglycerin' : N
  'portfinder' : {get-port}
  'record-http' : HttpRecorder
}


module.exports = ->

  @Given /^an ExoRelay instance$/, (done) ->
    get-port N (port) ~>
      @exo-relay = new ExoRelay exocomm-port: @exocomm-port
        ..listen port, done


  @Given /^an ExoRelay instance called "([^"]*)"$/, (instance-name) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port


  @Given /^an ExoRelay instance called "([^"]*)" listening at port (\d+)$/, (instance-name, port, done) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port
      ..listen port, done


  @Given /^an ExoRelay instance: "([^"]*)"$/, (code) ->
    eval "this.#{code}"


  @Given /^an ExoRelay instance listening at port (\d+)$/, (port, done) ->
    @exo-relay = new ExoRelay exocomm-port: @exocomm-port
      ..listen port, done


  @Given /^an ExoRelay instance with a handler for command "([^"]*)"$/, (command, done) ->
    @exo-relay = new ExoRelay!
      ..register-handler command, ->
      ..listen done


  @Given /^ExoComm runs at port (\d+)$/, (@exocomm-port, done) ->
    @exo-messaging = new HttpRecorder!listen @exocomm-port, done



  @Then /^the server crashes with the error "([^"]*)"$/, (expected-error) ->
    expect(@crashed).to.be.true
    expect(@crash-log).to.include expected-error


  @Then /^ExoRelay throws an exception with the message "([^"]*)"$/, (expected-message) ->
    expect(@error).to.equal expected-message
