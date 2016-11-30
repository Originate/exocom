require! {
  'chai' : {expect}
  'wait': {wait-until}
}


module.exports = ->

  @When /^I check the status$/, (done) ->
    @exocom.send service: 'test-service', name: '__status'
    wait-until (~> @exocom.received-messages.length), 1, ~>
      @status-code = @exocom.received-messages[0].name
      done!


  @Then /^ExoRelay connects to ExoCom$/ (done) ->
    @exo-relay
      ..connect!
    wait-until (~> @exocom.service-sockets[@service-name]), 1, ~>
      done!


  @Then /^it signals it is online$/, (done) ->
    wait-until (~> @status-code), 1, ~>
      expect(@status-code).to.equal '__status-ok'
      done!
