require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'record-http' : HttpRecorder
}


module.exports = ->

  @Given /^ExoCom runs at port (\d+)$/, (@exocom-port, done) ->
    @exocom = new HttpRecorder!listen @exocom-port, done



  @When /^I create an ExoRelay instance that uses ExoCom port (\d+)$/, (port) ->
    @exo-relay = new ExoRelay exocom-port: port, service-name: 'test'


  @When /^I try to create an ExoRelay without providing the ExoCom port$/, ->
    try
      @exo-relay = new ExoRelay service-name: 'test'
    catch
      @error = e.message


  @Then /^this instance uses the ExoCom port (\d+)$/, (+port) ->
    expect(@exo-relay.message-sender.exocom-port).to.equal port
