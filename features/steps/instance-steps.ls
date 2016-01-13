require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'nitroglycerin' : N
  'portfinder' : {get-port}
}


module.exports = ->

  @Given /^an ExoRelay instance$/, (done) ->
    get-port N (port) ~>
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



  @Then /^the server crashes with the error "([^"]*)"$/, (expected-error) ->
    expect(@crashed).to.be.true
    expect(@crash-log).to.include expected-error
