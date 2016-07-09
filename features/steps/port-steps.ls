require! {
  'chai' : {expect}
  'wait': {wait, wait-until}
}


module.exports = ->

  @When /^I take it online at the default port$/, (done) ->
    @exo-relay
      ..on 'online', -> done!
      ..listen!


  @When /^I take it online at port (\d+)$/, (port, done) ->
    @exo-relay
      ..on 'online', -> done!
      ..listen port


  @When /^I try to take it online at port "([^"]*)"$/, (port, done) ->
    @exo-relay
      ..on 'error', (@error) ~> done!
      ..on 'online', -> done 'should not be online'
      ..listen port



  @Then /^it is online at port (\d+)$/, (port, done) ->
    @exocom
      ..register-service name: 'test-service', port: port
      ..send service: 'test-service', name: '__status'
    wait-until (~> @exocom.received-messages.length), 1, ~>
      if @exocom.received-messages[0].name is "__status-ok"
        done!

