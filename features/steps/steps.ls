require! {
  'chai' : {expect}
  'nexpect'
  'path'
  'request'
}


module.exports = ->

  @When /^starting the "([^"]*)" example application$/, (app-name, done) ->
    @process = nexpect.spawn(path.join(process.cwd!, 'bin', 'exoservice-js'),
                             ['run'],
                             cwd: path.join(process.cwd!, 'features', 'example-apps', app-name),
                             strip-colors: yes)
                      .wait 'online at port', -> done!
                      .run (err) ->
                        throw err if err


  @Then /^the service runs at port (\d+)$/, (port, done) ->
    request "http://localhost:#{port}", done
