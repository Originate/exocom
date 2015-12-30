require! {
  'chai' : {expect}
  'nexpect'
  'path'
  'prelude-ls' : {head, tail}
  'request'
}


module.exports = ->

  @Given /^I am in the "([^"]*)" application directory$/, (@app-name) ->


  @When /^executing "([^"]*)"$/, (command, done) ->
    commands = command.split ' '
    @process = nexpect.spawn(path.join(process.cwd!, 'bin', head(commands)),
                             tail(commands),
                             cwd: path.join(process.cwd!, 'features', 'example-apps', @app-name),
                             strip-colors: yes)
                      .wait 'online at port', -> done!
                      .run (err) ->
                        throw err if err


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
