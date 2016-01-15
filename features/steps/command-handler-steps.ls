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


  @When /^I(?: try to)? register/, (code) ->
    try
      eval livescript.compile("@#{code}", bare: yes, header: no)
    catch
      @error = e.message


  @Then /^ExoRelay runs the registered handler, in this example calling "@([^"]*)" with "([^"]*)"$/, (command-name, command-argument, done) ->
    wait-until (~> @[command-name].called), 10, done


  @Then /^the instance has a handler for the command "([^"]*)"$/, (handler1) ->
    expect(@exo-relay.has-handler handler1).to.be.true


  @Then /^the instance has handlers for the commands "([^"]*)" and "([^"]*)"$/, (handler1, handler2) ->
    expect(@exo-relay.has-handler handler1).to.be.true
    expect(@exo-relay.has-handler handler2).to.be.true


  @Then /^the reply handler runs and in this example calls my "@([^"]*)" method with "([^"]*)"$/, (command-name, command-args, done) ->
    condition = ~>
      @[command-name].called and is-equal @print.first-call.args, [command-args]
    wait-until condition, 10, done
