require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'diff'
  'livescript'
  'request'
  'wait' : {wait, wait-until}
}


module.exports = ->


  @When /^I send out a .* command/, (code) ->
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
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exo-messaging.calls), 10, ~>
      wait 50, ~>
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
