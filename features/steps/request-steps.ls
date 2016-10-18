require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'ip'
  'jsdiff-console'
  'ejs'
  'livescript'
  'sinon'
  'wait' : {wait, wait-until}
}


module.exports = ->


  @When /^the reply arrives via this message:$/, (request-data) ->
    rendered = ejs.render request-data, request_uuid: @message-id
    eval livescript.compile "data = {\n#{rendered}\n}", bare: yes, header: no
    data.service = 'test-service'
    @exocom.send data


  @When /^I send a .*message/, (code) ->
    code = code.replace /\bexo-relay\b/, '@exo-relay'
    @message-id = eval livescript.compile(code, bare: yes, header: no)
    expect(@message-id).to.not.be.undefined


  @When /^receiving the "([^"]*)" message with payload "([^"]*)" as a reply to the "(?:[^"]*)" message$/, (message-name, payload, done) ->
    wait-until (~> @exocom.received-messages.length), 1, ~>
      exocom-data =
        service: 'test-service'
        name: message-name
        payload: payload
        id: '123'
        response-to: @exocom.received-messages[0].id
      @exocom
        ..reset!
        ..send exocom-data
      done!


  @When /^receiving this message:$/, (request-data) ->
    eval livescript.compile "data = {\n#{request-data}\n}", bare: yes, header: no
    data.service = 'test-service'
    @exocom.send data


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
    wait-until (~> @exocom.received-messages.length), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @message-id, ip_address: ip.address!
        template = livescript.compile "compiled = {\n#{rendered}\n}", bare: yes, header: no
        eval template
        jsdiff-console @exocom.received-messages[0], compiled, done


  @Then /^ExoRelay sends the "([^"]*)" message with payload "([^"]*)"$/, (message-name, payload, done) ->
    wait-until (~> @exocom.received-messages.length), 10, ~>
      wait 50, ~>
        expect(@exocom.received-messages[0].name).to.equal "#{message-name}"
        expect(@exocom.received-messages[0].payload).to.equal payload
        done!


  @Then /^my message handler (replies with|sends out) the message:$/ (request-data, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exocom.received-messages.length), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @exo-relay.message-sender.last-sent-id, ip_address: ip.address!
        template = "compiled = {\n#{rendered}\n}"
        compiled-template = livescript.compile template, bare: yes, header: no
        parsed = eval compiled-template
        jsdiff-console @exocom.received-messages[0], parsed, done
