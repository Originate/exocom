require! {
  'observable-process' : ObservableProcess
}


module.exports = ->

  @When /^I run "([^"]*)"$/, (code) ->
    @process = new ObservableProcess(code, verbose: yes)


  @Then /^this service runs at port (\d+)$/, (port, done) ->
    @process.wait "online at port #{port}", done

