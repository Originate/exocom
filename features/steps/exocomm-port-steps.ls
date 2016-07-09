require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'exocom-mock': MockExoCom
}


module.exports = ->

  @Given /^ExoCom runs at port (\d+)$/, (@exocom-port) ->
    @exocom or= new MockExoCom
      ..listen @exocom-port



  @When /^I create an ExoRelay instance that uses ExoCom port (\d+)$/, (port) ->
    @exo-relay = new ExoRelay exocom-port: port, service-name: 'test-service'


  @When /^I try to create an ExoRelay without providing the ExoCom port$/, ->
    try
      @exo-relay = new ExoRelay service-name: 'test-service'
    catch
      @error = e.message



  @Then /^this instance uses the ExoCom port (\d+)$/, (+port) ->
    expect(@exo-relay.message-sender.exocom-port).to.equal port
