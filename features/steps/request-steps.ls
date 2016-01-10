require! {
  'chai' : {expect}
  'chalk' : {green, grey, red}
  'diff'
  'ejs'
  'livescript'
  'request'
  'wait' : {wait, wait-until}
}


module.exports = ->


  @When /^I send out a .*command/, (code) ->
    @request-id = eval livescript.compile("@#{code}", bare: yes)
    expect(@request-id).to.not.be.undefined


  @When /^sending a POST request to "([^"]*)"$/, (url, done) ->
    options =
      method: 'POST'
      url: url
      json: yes
    request options, (err, @response, body) ~>
      expect(err).to.be.falsy
      done!


  @When /^the reply for this command arrives in the form of this incoming request:$/, (request-data, done) ->
    data = JSON.parse ejs.render(request-data, request_uuid: @request-id)
      ..json = yes
    request data, (err, response, body) ->
      expect(err).to.be.falsy
      expect(response.status-code).to.equal 200
      done!


  @Then /^it makes the requests:$/, (calls, done) ->
    # Wait until we get some call data, then wait another 50ms to let all the request data fill in
    wait-until (~> @exo-messaging.calls), 10, ~>
      wait 50, ~>
        changes = diff.diffJson @exo-messaging.calls, eval(ejs.render calls, request_uuid: @request-id)
        if changes.length is 1
          done!
        else
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
