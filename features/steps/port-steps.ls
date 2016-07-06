require! {
  'chai' : {expect}
  'request'
  'wait': {wait}
  'zmq'
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
    @exocom-listener.on 'message', (data) ~>
      if JSON.parse(data.to-string!).name is "__status-ok"
        done!
    @exocom-sender = zmq.socket 'push'
      ..connect "tcp://localhost:#{port}"
      ..send JSON.stringify name: '__status'

