require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'jsdiff-console'
  'ejs'
  'livescript'
  'request'
  'sinon'
  'wait' : {wait, wait-until}
}


module.exports = ->


  @When /^a reply for the sent message arrives via this incoming request:$/, (request-data, done) ->
    rendered = ejs.render request-data, request_uuid: @request-id
    eval livescript.compile "data = {\n#{rendered}\n}", bare: yes, header: no
    data.json = yes
    request data, (err, {status-code}) ->
      expect(err).to.be.null
      expect(status-code).to.equal 200
      done!


  @When /^I send a .*message/, (code) ->
    code = code.replace /\bexo-relay\b/, '@exo-relay'
    @request-id = eval livescript.compile(code, bare: yes, header: no)
    expect(@request-id).to.not.be.undefined


  @When /^receiving the "([^"]*)" message with payload "([^"]*)" as a reply to the "(?:[^"]*)" message$/, (message-name, payload, done) ->
    data =
      url: "http://localhost:4000/run/#{message-name}",
      method: 'POST'
      body:
        payload: payload
        requestId: '123'
        responseTo: @exocomm.calls[0].body.request-id
      json: yes
    @exocomm.reset!
    request data, (err, {status-code}) ->
      expect(err).to.be.null
      expect(status-code).to.equal 200
      done!


  @When /^receiving this message via the incoming request:$/, (request-data, done) ->
    eval livescript.compile "data = {\n#{request-data}\n}", bare: yes, header: no
    data.json = yes
    request data, (err, response, @response-body) ~>
      expect(err).to.be.null
      @response-code = response.status-code
      done err


  @When /^running this multi\-level request:$/, (code) ->
    done = @done = sinon.stub!
    eval livescript.compile code.replace(/\bexo-relay\b/g, '@exo-relay'), bare: yes, header: no


  @When /^sending the "([^"]*)" message:$/, (message-name, code) ->
    eval livescript.compile "@request-id = @#{code}", bare: yes, header: no


  @When /^trying to send/, (code) ->
    try
      eval livescript.compile "@#{code}", bare: yes, header: no
    catch
      @error = e.message



  @Then /^ExoRelay makes the request:$/, (request-data, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exocomm.calls?.length), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @request-id
        template = livescript.compile "compiled = {\n#{rendered}\n}", bare: yes, header: no
        eval template
        jsdiff-console @exocomm.calls, [compiled], done


  @Then /^ExoRelay returns a (\d+) response$/, (+expected-response-code) ->
    expect(@response-code).to.equal expected-response-code


  @Then /^ExoRelay returns a (\d+) response with the text "([^"]*)"$/, (+expected-response-code, expected-response-body) ->
    expect(@response-code).to.equal expected-response-code
    expect(@response-body).to.equal expected-response-body


  @Then /^ExoRelay sends the "([^"]*)" message with payload "([^"]*)"$/, (message-name, payload, done) ->
    wait-until (~> @exocomm.calls?.length), 10, ~>
      wait 50, ~>
        expect(@exocomm.calls).to.have.length 1
        call = @exocomm.calls[0]
        expect(call.url).to.equal "http://localhost:4010/send/#{message-name}"
        expect(call.body.payload).to.equal payload
        done!


  @Then /^my message handler (?:replies with|sends out)? a "([^"]*)" message(?: sent)? via this outgoing request:$/, (message-name, request-data, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exocomm.calls?.length), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @exo-relay.message-sender.last-sent-request-id
        template = "compiled = {\n#{rendered}\n}"
        compiled-template = livescript.compile template, bare: yes, header: no
        parsed = eval compiled-template
        jsdiff-console @exocomm.calls, [parsed], done
