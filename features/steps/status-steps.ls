require! {
  'chai' : {expect}
  'request'
  'zmq'
  'wait': {wait-until}
}


module.exports = ->

  @When /^I check the status$/, ->
    @exocom-listener.on 'message', (data) ~> @status-code = data
    @exocom-sender.send JSON.stringify name: '__status'



  @Then /^it signals it is online$/, ->
    wait-until (~> @status-code), 1, ~>
      expect(@status-code.name).to.equal '__status-ok'
