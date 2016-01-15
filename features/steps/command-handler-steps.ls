require! {
  'chai' : {expect}
  'livescript'
  'lodash.isequal' : is-equal
  'request'
  'sinon'
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^a hypothetical "@([^"]*)" command$/, (command-name) ->
    @[command-name] = sinon.stub!


  @Given /^I add many listeners at once:$/, (code) ->
    eval livescript.compile(code)


  @Given /^I register a handler for the "([^"]*)" command$/, (command-name) ->
    @hello-handler = sinon.stub!
    @exo-relay.register-handler command-name, @hello-handler


  @Given /^I register this handler for the "([^"]*)" command:$/, (command-name, code) ->
    code = "handler = #{code}"
    eval livescript.compile code, bare: yes, header: no
    @exo-relay.register-handler command-name, handler


  @Given /^I try to set up this handler:$/, (code) ->
    try
      eval livescript.compile "@#{code}", bare: yes, header: no
    catch
      @error = e.message

  @Given /^my ExoRelay instance already has a handler for the command "([^"]*)"$/, (command-name) ->
    @exo-relay.register-handler command-name, ->


  @Given /^the "([^"]*)" command has this handler:$/, (command-name, handler-code) ->
    eval livescript.compile "@#{handler-code}", bare: yes, header: no



  @When /^I try to add another handler for that command$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @error = e.message


  @When /^I try to add the same command listener$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @crashed = yes
      @crash-log = e.stack


  @Given /^I(?: try to)? register/, (code) ->
    try
      eval livescript.compile("@#{code}", bare: yes, header: no)
    catch
      @error = e.message


  @When /^receiving the request:$/, (request-data, done) ->
    request-data = JSON.parse request-data
    request-data.json = yes
    request request-data, done


  @Then /^ExoRelay runs the registered handler, in this example calling "@([^"]*)" with "([^"]*)"$/, (command-name, command-argument, done) ->
    wait-until (~> @[command-name].called is yes), 10, done


  @Then /^it calls the registered "([^"]*)" handler$/, (arg1, done) ->
    wait-until (~> @hello-handler.called is yes), ~>
      expect(@hello-handler.calledOnce).to.be.true
      done!


  @Then /^it calls the registered "([^"]*)" handler with:$/, (command-name, table, done) ->
    wait-until (~> @hello-handler.called is yes), ~>
      expect(@hello-handler.calledOnce).to.be.true
      eval livescript.compile("expected-args = {#{table.rows-hash!PAYLOAD}}", bare: yes, header: no)
      expect(@hello-handler.first-call.args).to.eql [expected-args]
      done!


  @Then /^the instance has a handler for the command "([^"]*)"$/, (handler1) ->
    expect(@exo-relay.has-handler handler1).to.be.true


  @Then /^the instance has handlers for the commands "([^"]*)" and "([^"]*)"$/, (handler1, handler2) ->
    expect(@exo-relay.has-handler handler1).to.be.true
    expect(@exo-relay.has-handler handler2).to.be.true


  @Then /^this command handler gets called$/, (done) ->
    wait-until (~> @ran), 10, done


  @Then /^the reply handler runs and in this example calls my "@([^"]*)" method with "([^"]*)"$/, (command-name, command-args, done) ->
    condition = ~>
      @[command-name].called and is-equal @print.first-call.args, [command-args]
    wait-until condition, 10, done


  @Then /^the reply handler is called, meaning:$/, (code) ->
    expect(eval livescript.compile(code, bare: yes)).to.be.true
