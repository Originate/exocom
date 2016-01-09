require! {
  'chai' : {expect}
  'diff'
  'livescript'
  'request'
}


module.exports = ->


  @When /^I send out a .* command/, (code, done) ->
    eval livescript.compile "@#{code}"


  @When /^sending a POST request to "([^"]*)"$/, (url, done) ->
    options =
      method: 'POST'
      url: url
      json: yes
    request options, (err, @response, body) ~>
      expect(err).to.be.falsy
      done!



  @Then /^it makes the requests:$/, (calls, done) ->
    changes = diff.diffJson @exo-messaging.calls, eval(calls)
    return done! if changes.length is 1
    console.log red '\nxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    console.log red 'Mismatching call records!\n'
    for part in changes
      color = switch
      | part.added    =>  green
      | part.removed  =>  red
      | _             =>  grey
      process.stdout.write color part.value
    console.log red '\n\nxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n'
    done 'Mismatching recorded calls, see above'
