require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'diff'
  'ejs'
  'livescript'
  'request'
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


  @When /^receiving this command via the incoming request:$/, (request-data, done) ->
    eval livescript.compile "data = {\n#{request-data}\n}", bare: yes, header: no
    data.json = yes
    request data, done


  @When /^sending a POST request to "([^"]*)"$/, (url, done) ->
    options =
      method: 'POST'
      url: url
      json: yes
    request options, (err, @response, body) ~>
      expect(err).to.be.falsy
      done!


  @When /^sending the "([^"]*)" command:$/, (command-name, code) ->
    eval livescript.compile "@request-id = @#{code}", bare: yes, header: no


  @When /^the reply for this command arrives in the form of this incoming request:$/, (request-data, done) ->
    data = JSON.parse ejs.render(request-data, request_uuid: @request-id)
      ..json = yes
    request data, (err, response, body) ->
      expect(err).to.be.falsy
      expect(response.status-code).to.equal 200
      done!


  @When /^trying to send/, (code) ->
    try
      eval livescript.compile "@#{code}", bare: yes, header: no
    catch
      @error = e.message



  @Then /^ExoRelay makes the request:$/, (request-data, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exo-messaging.calls?.length), 10, ~>
      wait 50, ~>

        rendered = ejs.render request-data, request_uuid: @request-id
        template = livescript.compile "compiled = {\n#{rendered}\n}", bare: yes, header: no
        eval template
        changes = diff.diffJson @exo-messaging.calls, [compiled]
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


  @Then /^it makes the requests:$/, (calls, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exo-messaging.calls), 10, ~>
      wait 50, ~>
        changes = diff.diffJson @exo-messaging.calls, eval(ejs.render calls, request_uuid: @request-id)
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


  @Then /^my command handler replies with a "([^"]*)" command sent via this outgoing request:$/, (command-name, request-data, done) ->

    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exo-messaging.calls?.length), 10, ~>
      wait 50, ~>
        rendered = ejs.render request-data, request_uuid: @exo-relay.command-sender.last-sent-request-id
        template = "compiled = {\n#{rendered}\n}"
        compiled-template = livescript.compile template, bare: yes, header: no
        parsed = eval compiled-template
        changes = diff.diffJson @exo-messaging.calls, [parsed]
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
