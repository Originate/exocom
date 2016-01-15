require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'diff'
  'ejs'
  'livescript'
  'request'
  'sinon'
  'wait' : {wait, wait-until}
}


module.exports = ->


  @When /^a reply for the sent command arrives via this incoming request:$/, (request-data, done) ->
    rendered = ejs.render request-data, request_uuid: @request-id
    eval livescript.compile "data = {\n#{rendered}\n}", bare: yes, header: no
    data.json = yes
    request data, done


  @When /^I send a .*command/, (code) ->
    code = code.replace /\bexo-relay\b/, '@exo-relay'
    @request-id = eval livescript.compile(code, bare: yes, header: no)
    expect(@request-id).to.not.be.undefined


  @When /^receiving the "([^"]*)" command with payload "([^"]*)" as a reply to the "([^"]*)" command$/, (command-name, payload, done) ->
    data =
      url: "http://localhost:4000/run/#{command-name}",
      method: 'POST'
      body:
        payload: payload
        requestId: '123'
        responseTo: @exocomm.calls[0].body.request-id
      json: yes
    @exocomm.reset!
    request data, done


  @When /^receiving this command via the incoming request:$/, (request-data, done) ->
    eval livescript.compile "data = {\n#{request-data}\n}", bare: yes, header: no
    data.json = yes
    request data, done


  @When /^running this multi\-level request:$/, (code) ->
    done = @done = sinon.stub!
    eval livescript.compile code.replace(/\bexo-relay\b/g, '@exo-relay'), bare: yes, header: no


  @When /^sending the "([^"]*)" command:$/, (command-name, code) ->
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
        diff-json @exocomm.calls, [compiled], done


  @Then /^ExoRelay sends the "([^"]*)" command with payload "([^"]*)"$/, (command-name, payload, done) ->
    wait-until (~> @exocomm.calls?.length), 10, ~>
      wait 50, ~>
        expect(@exocomm.calls).to.have.length 1
        call = @exocomm.calls[0]
        expect(call.url).to.equal "http://localhost:4010/send/#{command-name}"
        expect(call.body.payload).to.equal payload
        done!


  @Then /^my command handler replies with a "([^"]*)" command sent via this outgoing request:$/, (command-name, request-data, done) ->

    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exocomm.calls?.length), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @exo-relay.command-sender.last-sent-request-id
        template = "compiled = {\n#{rendered}\n}"
        compiled-template = livescript.compile template, bare: yes, header: no
        parsed = eval compiled-template
        diff-json @exocomm.calls, [parsed], done



diff-json = (expected, actual, done) ->
  changes = diff.diffJson expected, actual
  if changes.length is 1
    done!
  else
    console.log red '\nxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    console.log red 'Mismatching call records!\n'
    for part in changes
      color = switch
      | part.added    =>  green
      | part.removed  =>  red
      | _             =>  grey
      process.stdout.write color part.value
    console.log red '\n\nxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n'
    done 'Mismatching recorded calls, see above'
