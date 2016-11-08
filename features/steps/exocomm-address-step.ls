require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'exocom-mock': MockExoCom
  'wait' : {wait}
}


module.exports = ->

  @Given /^ExoCom runs at port (\d+)$/, (@exocom-port) ->
    @exocom = new MockExoCom
      ..listen @exocom-port


  @When /^an ExoCom instance comes online (\d+) second(?:s)? later$/ timeout: 10_000, (seconds, done) ->
    wait seconds * 1000, ~>
      @exocom = new MockExoCom
        ..listen 4100
      done!


  @When /^creating an ExoRelay instance using ExoCom host "([^"]*)" and port (\d+)$/ (host, +port) ->
    @exo-relay = new ExoRelay do
      exocom-host: host
      exocom-port: port
      service-name: 'test-service'


  @When /^trying to create an ExoRelay without providing the ExoCom port$/, ->
    try
      @exo-relay = new ExoRelay do
        exocom-host: 'localhost'
        service-name: 'test-service'
    catch
      @error = e.message


  @When /^trying to create an ExoRelay without providing the ExoCom host$/, ->
    try
      @exo-relay = new ExoRelay do
        exocom-port: 4100
        service-name: 'test-service'
    catch
      @error = e.message


  @Then /^this instance uses the ExoCom host "([^"]*)" and port (\d+)$/ (host, +port) ->
    expect(@exo-relay.websocket-connector.exocom-port).to.equal port
    expect(@exo-relay.websocket-connector.exocom-host).to.equal host
