require! {
  'chai' : {expect}
  'http'
  'nitroglycerin' : N
  'portfinder'
  'request'
  '../support/text-tools' : {ascii}
  'wait' : {wait}
}


module.exports = ->

  @Given /^a (.*?) instance registered to send "([^"]*)"$/, (name, command, done) ->
    @instance-for-sending {name, command}, done


  @Given /^a (.*?) instance registered to receive "([^"]*)"$/, (name, command, done) ->
    @instance-for-receiving {name, command}, done


  @Given /^an ExoComm instance$/, (done) ->
    portfinder.get-port N (@exocomm-port) ~>
      @create-exocomm-instance port: @exocomm-port, done


  @Given /^another service already uses port (\d+)$/, (+port, done) ->
    wait 100, ~>   # to let the previous server shut down
      handler = (_, res) -> res.end 'existing server'
      @existing-server = http.create-server(handler).listen port, done


  @Given /^(?:ExoComm|it) knows about these services:$/, (table, done) ->
    @register-service parse-service-data-table(table), done



  @When /^I( try to)? run ExoComm at port (\d+)$/, (!!expect-error, +port, done) ->
    @run-exocomm-at-port port, expect-error, done


  @When /^I( try to)? run ExoComm at the default port$/, (!!expect-error, done) ->
    @run-exocomm expect-error, done


  @When /^receiving a registration for this service:$/, (table, done) ->
    @register-service parse-service-data-table(table), done


  @When /^the (.*?) sends "([^"]*)"$/, (service, command, done) ->
    set-immediate ~>
      @last-sent-message = command
      @service-sends-command service, command, done



  @Then /^ExoComm broadcasts this message to the (.*?)$/, (service-name, done) ->
    @verify-sent-calls {service-name, message: @last-sent-message}, done


  @Then /^it aborts with the message "([^"]*)"$/, (message, done) ->
    @verify-abort-with-message message, done


  @Then /^it knows about these services now:$/, (table, done) ->
    @verify-knows-about-services parse-service-data-table(table), done


  @Then /^it runs at port (\d+)$/, (+port, done) ->
    @verify-runs-at-port port, done



parse-service-data-table = (table) ->
  result = [{[key.to-lower-case!, value] for key, value of row} for row in table.hashes!]
  for row in result
    row.sends = row.sends.split ', '
    row.receives = row.receives.split ', '
  result
