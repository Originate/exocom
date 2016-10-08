require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'exocom-mock': MockExoCom
}


module.exports = ->

  @Given /^ExoCom runs at port (\d+)$/, (@exocom-port) ->
    @exocom = new MockExoCom
      ..listen @exocom-port


  @When /^creating an ExoRelay instance using ExoCom host "([^"]*)" and port (\d+)$/ (host, +port) ->
    @exo-relay = new ExoRelay exocom-host: host, exocom-port: port, service-name: 'test-service'


  @When /^trying to create an ExoRelay without providing the ExoCom port$/, ->
    try
      @exo-relay = new ExoRelay exocom-host: 'localhost', service-name: 'test-service'
    catch
      @error = e.message


  @When /^trying to create an ExoRelay without providing the ExoCom host$/ ->
    try
      @exo-relay = new ExoRelay exocom-port: 4100, service-name: 'test-service'
    catch
      @error = e.message

  @Then /^this instance uses the ExoCom host "([^"]*)" and port (\d+)$/ (host, +port) ->
    expect(@exo-relay.message-sender.exocom-port).to.equal port
    expect(@exo-relay.message-sender.exocom-host).to.equal host
