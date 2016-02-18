require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'record-http' : HttpRecorder
}


module.exports = ->

  @Given /^ExoComm runs at port (\d+)$/, (@exocomm-port, done) ->
    @exocomm = new HttpRecorder!listen @exocomm-port, done



  @When /^I create an ExoRelay instance that uses ExoComm port (\d+)$/, (port) ->
    @exo-relay = new ExoRelay exocomm-port: port


  @When /^I try to create an ExoRelay without providing the ExoComm port$/, ->
    try
      @exo-relay = new ExoRelay
    catch
      @error = e.message


  @Then /^this instance uses the ExoComm port (\d+)$/, (+port) ->
    expect(@exo-relay.message-sender.exocomm-port).to.equal port
