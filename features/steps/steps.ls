require! {
  '../..' : ExoRelay
  'chai' : {expect}
  'chalk' : {red, green, grey}
  'diff'
  '../example-apps/example-apps'
  'livescript'
  'nitroglycerin' : N
  'portfinder' : {get-port}
  'record-http' : HttpRecorder
  'request'
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^an ExoRelay instance$/, (done) ->
    get-port N (port) ~>
      @exo-relay = new ExoRelay exo-messaging-port: @exo-messaging-port
        ..listen port, done


  @Given /^an ExoRelay instance: "([^"]*)"$/, (code) ->
    eval "this.#{code}"


  @Given /^an ExoRelay instance listening at port (\d+)$/, (port, done) ->
    @exo-relay = new ExoRelay!
      ..listen port, done


  @Given /^an ExoRelay instance with a handler for command "([^"]*)"$/, (command, done) ->
    get-port N (port) ~>
      @exo-relay = new ExoRelay!
        ..register-handler command, ->
        ..listen port, done


  @Given /^I add a command listener:$/, (code) ->
    eval livescript.compile "@#{code}"


  @When /^I try to add the same command listener$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @crashed = yes
      @crash-log = e.stack


  @Given /^the Exosphere messaging infrastructure runs at port (\d+)$/, (@exo-messaging-port, done) ->
    @exo-messaging = new HttpRecorder!listen @exo-messaging-port, done



  @When /^I send out a .* command/, (code, done) ->
    eval livescript.compile "@#{code}"


  @When /^I take it online at port (\d+): "([^"]*)"$/, (port, code, done) ->
    eval "this.#{code}"


  @When /^sending a POST request to "([^"]*)"$/, (url, done) ->
    options =
      method: 'POST'
      url: url
      json: yes
    request options, (err, @response, body) ~>
      expect(err).to.be.falsy
      done!



  @Then /^it is online at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}/run", (err, response, body) ->
      expect(err).to.be.falsy
      expect(response.status-code).to.equal 200
      done!


  @Then /^it makes the requests:$/, (calls, done) ->
    changes = diff.diffJson @exo-messaging.calls, eval(calls)
    return done! if changes.length is 1
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


  @Then /^the server crashes with the error "([^"]*)"$/, (expected-error) ->
    expect(@crashed).to.be.true
    expect(@crash-log).to.include expected-error


  @Then /^this command handler gets called$/, (done) ->
    wait-until (~> @ran), 10, done
