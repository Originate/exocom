require! {
  'chai' : {expect}
  'prelude-ls' : {any}
  'wait': {wait, wait-until}
}


module.exports = ->

  @When /^I take it online at the default port$/, (done) ->
    @exo-relay
      ..on 'online', -> done!
      ..listen!


  @When /^I take it online at port (\d+)$/, (port, done) ->
    @exo-relay
      ..on 'online', ~>
        wait-until (~> @exocom.received-messages.length), 10, ~>
          @exocom.reset!
          done!
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
    current-length = @exocom.received-messages.length
    wait-until (~> @exocom.received-messages.length > current-length), 1, ~>
      if @exocom.received-messages |> any (.name is "__status-ok")
        done!
