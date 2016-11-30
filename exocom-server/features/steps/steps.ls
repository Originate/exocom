require! {
  'chai' : {expect}
  'http'
  'livescript'
  '../support/mock-service' : MockService
  'nitroglycerin' : N
  'port-reservation'
  'request'
  '../support/text-tools' : {ascii}
  'wait' : {wait}
}


module.exports = ->

  @Given /^a running "([^"]*)" instance$/, (name, done) ->
    @create-mock-service-at-port {name, port: @exocom-port}, ->
      wait 200, done


  @Given /^an ExoCom instance$/, (done) ->
    port-reservation
      ..base-port = 5000
      ..get-port N (@exocom-port) ~>
        @create-exocom-instance port: @exocom-port, done


  @Given /^an ExoCom instance configured with the routes:?$/, (service-messages, done) ->
    service-messages = service-messages |> (.replace /\s/g, '') |> (.replace /"/g, '')
    port-reservation
      ..base-port = 5000
      ..get-port N (@exocom-port) ~>
        @create-exocom-instance port: @exocom-port, service-messages: service-messages, done


  @Given /^an ExoCom instance managing the service landscape:$/ (table, done) ->
    port-reservation
      ..base-port = 5000
      ..get-port N (@exocom-port) ~>
        @create-exocom-instance port: @exocom-port, ~>
          @create-mock-service-at-port {name: service.NAME, port: @exocom-port}, ->
          done!


  @Given /^another service already uses port (\d+)$/, (+port, done) ->
    wait 100, ~>   # to let the previous server shut down
      handler = (_, res) -> res.end 'existing server'
      @existing-server = http.create-server(handler).listen port, done


  @Given /^the "([^"]+)" service sends "([^"]*)" with id "([^"]*)"$/, (service, message, id, done) ->
    @last-sent-message = message
    @last-sent-message-id = id
    @service-sends-message {service, message, id}, done


  @When /^the "([^"]*)" service goes offline$/ (service-name, done) ->
    @service-mocks[service-name].close!
    wait 200, done


  @When /^a new "([^"]*)" service$/ (name, done) ->
    @create-mock-service-at-port {name, port: @exocom-port}, done


  @When /^(I try )?starting ExoCom at port (\d+)$/, (!!expect-error, +port, done) ->
    @run-exocom-at-port port, expect-error, done


  @When /^I( try to)? run ExoCom$/, (!!expect-error, done) ->
    @run-exocom-at-port null, expect-error, done


  @When /^requesting the routing information$/, (done) ->
    @get-routing-information (@routing-information) ~>
      done!


  @When /^sending the service configuration:$/, (config-text, done) ->
    eval livescript.compile "config = \n#{config-text.replace /^/gm, '  '}", bare: yes, header: no
    @set-service-landscape config, done


  @When /^the "([^"]+)" service sends "([^"]*)"$/, (service, message, done) ->
    @last-sent-message = message
    @service-sends-message {service, message}, done


  @When /^the "([^"]+)" service sends "([^"]*)" in reply to "([^"]*)"$/, (service, reply-message, response-to, done) ->
    @last-sent-message = reply-message
    @service-sends-reply {service, message: reply-message, response-to}, done


  @Then /^ExoCom broadcasts the message "([^"]*)" to the "([^"]+)" service$/, (message, service-name, done) ->
    @verify-sent-calls {service-name, message: message, id: @last-sent-message-id}, done


  @Then /^ExoCom broadcasts the reply "([^"]*)" to the "([^"]+)" service$/, (message, service-name, done) ->
    @verify-sent-calls {service-name, message: message, response-to: '111'}, done


  @Then /^ExoCom now knows about these services:$/ (table, done) ->
    services = {}
    for row in table.hashes!
      services[row.NAME] =
        name: row.NAME
        internal-namespace: row['INTERNAL NAMESPACE']
    @verify-service-setup services, done


  @Then /^ExoCom signals the error "([^"]*)"$/, (message, done) ->
    @process.wait message, done


  @Then /^ExoCom signals "([^"]*)"$/, (message, done) ->
    @verify-exocom-signaled-string message, done


  @Then /^ExoCom signals that this message was sent$/, (done) ->
    @verify-exocom-broadcasted-message message: @last-sent-message, done


  @Then /^ExoCom signals that this reply is sent from the ([^ ]+) to the (.+)$/, (sender, receiver, done) ->
    @verify-exocom-broadcasted-message message: @last-sent-message, sender: sender, receivers: [receiver], response-to: '111', done


  @Then /^ExoCom signals that this reply was sent$/, (done) ->
    @verify-exocom-broadcasted-reply @last-sent-message, done


  @Then /^it aborts with the message "([^"]*)"$/, (message, done) ->
    @verify-abort-with-message message, done


  @Then /^it has this routing table:$/, (table, done) ->
    expected-routes = {}
    for row in table.hashes!
      eval livescript.compile "receiver-json = {#{row.RECEIVERS}}", bare: yes, header: no
      expected-routes[row.MESSAGE] =
        receivers: [receiver-json]
    @verify-routing-setup expected-routes, done


  @Then /^it opens a port at (\d+)$/, (+port, done) ->
    @verify-listening-at-ports port, done


  @Then /^it opens an HTTP listener at port (\d+)$/, (+port, done) ->
    @verify-listening-at-ports port, done
