require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'jsdiff-console'
  'ejs'
  'livescript'
  'request'
  'sinon'
  'wait' : {wait, wait-until}
  'zmq'
}


module.exports = ->


  @When /^the reply arrives via this message:$/, (request-data) ->
    rendered = ejs.render request-data, request_uuid: @message-id
    eval livescript.compile "data = {\n#{rendered}\n}", bare: yes, header: no
    @exocom-sender.send JSON.stringify data


  @When /^I send a .*message/, (code) ->
    code = code.replace /\bexo-relay\b/, '@exo-relay'
    @message-id = eval livescript.compile(code, bare: yes, header: no)
    expect(@message-id).to.not.be.undefined


  @When /^receiving the "([^"]*)" message with payload "([^"]*)" as a reply to the "(?:[^"]*)" message$/, (message-name, payload) ->
    wait-until (~> @exorelay-message), 1, ~>
      exocom-data =
        message: message-name
        payload: payload
        id: '123'
        response-to: @exorelay-message.id
      @exorelay-message = null
      @exocom-sender.send JSON.stringify exocom-data


  @When /^receiving this message:$/, (request-data) ->
    eval livescript.compile "data = {\n#{request-data}\n}", bare: yes, header: no
    @exocom-sender.send JSON.stringify data


  @When /^running this multi\-level request:$/, (code) ->
    done = @done = sinon.stub!
    eval livescript.compile code.replace(/\bexo-relay\b/g, '@exo-relay'), bare: yes, header: no


  @When /^sending the message:$/, (code) ->
    eval livescript.compile "@message-id = @#{code}", bare: yes, header: no


  @When /^trying to send/, (code) ->
    try
      eval livescript.compile "@#{code}", bare: yes, header: no
    catch
      @error = e.message



  @Then /^ExoRelay makes the ZMQ request:$/, (request-data, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exorelay-message), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @message-id
        template = livescript.compile "compiled = {\n#{rendered}\n}", bare: yes, header: no
        eval template
        jsdiff-console @exorelay-message, compiled, done


  @Then /^ExoRelay sends the "([^"]*)" message with payload "([^"]*)"$/, (message-name, payload, done) ->
    wait-until (~> @exorelay-message), 10, ~>
      wait 50, ~>
        expect(@exorelay-message.name).to.equal "#{message-name}"
        expect(@exorelay-message.payload).to.equal payload
        done!


  @Then /^my message handler (replies with|sends out) the message:$/ (request-data, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exorelay-message), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @exo-relay.message-sender.last-sent-id
        template = "compiled = {\n#{rendered}\n}"
        compiled-template = livescript.compile template, bare: yes, header: no
        parsed = eval compiled-template
        jsdiff-console @exorelay-message, parsed, done
