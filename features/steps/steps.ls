require! {
  'chai' : {expect}
  'nexpect' : {spawn}
  'path'
  'prelude-ls' : {head, tail}
  'request'
}


module.exports = ->

  @Given /^I am in the "([^"]*)" service directory$/, (@app-name) ->


  @When /^executing "([^"]*)"$/, (command, done) ->
    command-parts = command.split ' '
    @process = spawn(path.join(process.cwd!, 'bin', head(command-parts)),
                             tail(command-parts),
                             cwd: path.join(process.cwd!, 'features', 'example-apps', @app-name),
                             strip-colors: yes)
                 .wait 'online at port', -> done!
                 .run (err) ->
                   throw err if err


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
