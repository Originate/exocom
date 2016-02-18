require! {
  'chai' : {expect}
  'livescript'
  'lodash.isequal' : is-equal
  'request'
  'sinon'
  'wait' : {wait-until}
}


module.exports = ->


  @Given /^a hypothetical "([^"]*)" message$/, (message-name) ->
    global[message-name] = sinon.stub!


  @Given /^I register this handler for the "([^"]*)" message:$/, (message-name, code) ->
    code = "handler = #{code}"
    eval livescript.compile code, bare: yes, header: no
    @exo-relay.register-handler message-name, handler


  @Given /^I try to set up this handler:$/, (code) ->
    try
      eval livescript.compile "@#{code}", bare: yes, header: no
    catch
      @error = e.message


  @Given /^my ExoRelay instance already has a handler for the message "([^"]*)"$/, (message-name) ->
    @exo-relay.register-handler message-name, ->


  @Given /^the "([^"]*)" message has this handler:$/, (message-name, handler-code) ->
    eval livescript.compile "@#{handler-code}", bare: yes, header: no



  @When /^I try to add another handler for that message$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @error = e.message


  @When /^I(?: try to)? register/, (code) ->
    try
      eval livescript.compile("@#{code}", bare: yes, header: no)
    catch
      @error = e.message


  @Then /^(?:ExoRelay|it) runs the registered handler, in this example calling "([^"]*)" with "([^"]*)"$/, (message-name, message-argument, done) ->
    wait-until (~> global[message-name].called), 10, done


  @Then /^the instance has a handler for the message "([^"]*)"$/, (handler1) ->
    expect(@exo-relay.has-handler handler1).to.be.true


  @Then /^the instance has handlers for the messages "([^"]*)" and "([^"]*)"$/, (handler1, handler2) ->
    expect(@exo-relay.has-handler handler1).to.be.true
    expect(@exo-relay.has-handler handler2).to.be.true


  @Then /^the reply handler runs and in this example calls my "([^"]*)" method with "([^"]*)"$/, (message-name, message-args, done) ->
    condition = ~>
      global[message-name].called and is-equal global[message-name].first-call.args, [message-args]
    wait-until condition, 10, done
