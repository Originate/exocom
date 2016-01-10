require! {
  'chai' : {expect}
  'livescript'
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^I add a command listener:$/, (code) ->
    eval livescript.compile(code)


  @Given /^I add many listeners at once:$/, (code) ->
    eval livescript.compile(code)



  @When /^I try to add the same command listener$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @crashed = yes
      @crash-log = e.stack



  @Then /^the instance has a handler for the command "([^"]*)"$/, (handler1) ->
    expect(@exo-relay.has-handler handler1).to.be.true


  @Then /^the instance has handlers for the commands "([^"]*)" and "([^"]*)"$/, (handler1, handler2) ->
    expect(@exo-relay.has-handler handler1).to.be.true
    expect(@exo-relay.has-handler handler2).to.be.true


  @Then /^this command handler gets called$/, (done) ->
    wait-until (~> @ran), 10, done


  @Then /^the reply handler is called, meaning:$/, (code) ->
    expect(eval livescript.compile(code, bare: yes)).to.be.true
