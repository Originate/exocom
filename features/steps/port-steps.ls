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


  @When /^I take it online$/, (done) ->
    @exo-relay
      ..on 'online', ~>
        wait-until (~> @exocom.service-sockets['test-service']), 10, ~>
          done!


  @Then /^it is online$/, (done) ->
    @exocom
      ..send service: 'test-service', name: '__status'
    current-length = @exocom.received-messages.length
    wait-until (~> @exocom.received-messages.length > current-length), 1, ~>
      if @exocom.received-messages |> any (.name is "__status-ok")
        done!
