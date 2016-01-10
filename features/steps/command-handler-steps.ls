require! {
  'chai' : {expect}
  'livescript'
  'request'
  'sinon'
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^I add a command listener:$/, (code) ->
    eval livescript.compile(code)


  @Given /^I add many listeners at once:$/, (code) ->
    eval livescript.compile(code)


  @Given /^I register a handler for the "([^"]*)" command$/, (command-name) ->
    @hello-handler = sinon.stub!
    @exo-relay.register-handler command-name, @hello-handler



  @When /^I try to add the same command listener$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @crashed = yes
      @crash-log = e.stack


  @When /^receiving the request:$/, (request-data, done) ->
    request-data = JSON.parse request-data
    request-data.json = yes
    request request-data, done


  @Then /^it calls the registered "([^"]*)" handler$/, (arg1, done) ->
    wait-until (~> @hello-handler.called is yes), done


  @Then /^the instance has a handler for the command "([^"]*)"$/, (handler1) ->
    expect(@exo-relay.has-handler handler1).to.be.true


  @Then /^the instance has handlers for the commands "([^"]*)" and "([^"]*)"$/, (handler1, handler2) ->
    expect(@exo-relay.has-handler handler1).to.be.true
    expect(@exo-relay.has-handler handler2).to.be.true


  @Then /^this command handler gets called$/, (done) ->
    wait-until (~> @ran), 10, done


  @Then /^the reply handler is called, meaning:$/, (code) ->
    expect(eval livescript.compile(code, bare: yes)).to.be.true
