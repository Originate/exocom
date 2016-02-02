require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'record-http' : HttpRecorder
}


module.exports = ->

  @Given /^ExoComm runs at port (\d+)$/, (@exocomm-port, done) ->
    @exocomm = new HttpRecorder!listen @exocomm-port, done



  @When /^I create an ExoRelay instance that uses the default ExoComm port$/, ->
    @exo-relay = new ExoRelay


  @When /^I create an ExoRelay instance that uses ExoComm port (\d+)$/, (port) ->
    @exo-relay = new ExoRelay exocomm-port: port



  @Then /^this instance uses the ExoComm port (\d+)$/, (+port) ->
    expect(@exo-relay.command-sender.exocomm-port).to.equal port
