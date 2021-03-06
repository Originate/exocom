require! {
  'chai' : {expect}
  'cucumber': {defineSupportCode}
  'wait' : {wait-until}
}


defineSupportCode ({Then}) ->

  Then /^after a while it sends the "([^"]*)" message(?: with auth "([^"]*)")?$/, (reply-message-name, auth, done) ->
    @exocom.on-receive ~>
      received-messages = @exocom.received-messages
      expect(received-messages).to.have.length 1
      expect(received-messages[0].name).to.equal reply-message-name
      expect(received-messages[0].auth).to.equal auth
      done!


  Then /^after a while it sends the "([^"]*)" message with the textual payload:$/, (reply-message-name, payload-text, done) ->
    @exocom.on-receive ~>
      received-messages = @exocom.received-messages
      expect(received-messages).to.have.length 1
      expect(received-messages[0].name).to.equal reply-message-name
      expect(received-messages[0].payload).to.equal payload-text
      done!


  Then /^it acknowledges the received message$/, (done) ->
    wait-until (~> @exocom.received-messages.length), done


  Then /^it connects to the ExoCom instance$/, (done) ->
    @exocom.send @role, name: '__status' , id: '123'
    wait-until (~> @exocom.received-messages[0]), 1, ~>
      if @exocom.received-messages[0].name is "__status-ok"
        done!


  Then /^it runs the "([^"]*)" hook$/, (hook-name, done) ->
    @exocom
      ..reset!
      ..send @role, name: 'which-hooks-ran'
      ..on-receive ~>
        expect(@exocom.received-messages[0].payload).to.eql ['before-all']
        done!
