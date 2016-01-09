require! {
  'livescript'
  'wait' : {wait-until}
}


module.exports = ->

  @Given /^I add a command listener:$/, (code) ->
    eval livescript.compile "@#{code}"


  @When /^I try to add the same command listener$/, ->
    try
      @exo-relay.register-handler 'hello', ->
    catch
      @crashed = yes
      @crash-log = e.stack



  @Then /^this command handler gets called$/, (done) ->
    wait-until (~> @ran), 10, done
