require! {
  'chai' : {expect}
  'http'
  'livescript'
  'nitroglycerin' : N
  'port-reservation'
  'request'
  '../support/text-tools' : {ascii}
  'wait' : {wait}
}


module.exports = ->

  @Given /^a "([^"]*)" instance running at port (\d+)$/, (name, port, done) ->
    @create-instance-at-port name, port, done
    (@ports or= {})[name] = port


  @Given /^an ExoCom instance$/, (done) ->
    port-reservation
      ..base-port = 5000
      ..get-port N (@exocom-port) ~>
        @create-exocom-instance port: @exocom-port, done


  @Given /^an ExoCom instance configured for the service landscape:$/, (table, done) ->
    port-reservation
      ..base-port = 5000
      ..get-port N (@exocom-port) ~>
        @create-exocom-instance port: @exocom-port, ~>
          data = for service in table.hashes!
            {
              name: service.NAME
              internal-namespace: service['INTERNAL NAMESPACE']
              host: service.HOST
              port: +service.PORT
              sends: service.SENDS.split(',')
              receives: service.RECEIVES.split(' ')
            }
          @set-service-landscape data, done


  @Given /^another service already uses port (\d+)$/, (+port, done) ->
    wait 100, ~>   # to let the previous server shut down
      handler = (_, res) -> res.end 'existing server'
      @existing-server = http.create-server(handler).listen port, done


  @Given /^the "([^"]+)" service sends "([^"]*)" with id "([^"]*)"$/, (service, message, id, done) ->
    @last-sent-message = message
    @last-sent-message-id = id
    @service-sends-message {service, message, id}, done


  @When /^I( try to)? run ExoCom at port (\d+)$/, (!!expect-error, +port, done) ->
    @run-exocom-at-port port, expect-error, done


  @When /^requesting the routing information$/, (done) ->
    @get-routing-information (@routing-information) ~>
      done!


  @When /^setting this service landscape:$/, (table, done) ->
    data = for service in table.hashes!
      {
        name: service.NAME
        host: service.HOST
        port: +service.PORT
        sends: service.SENDS.split(',')
        receives: service.RECEIVES.split(' ')
      }
    @set-service-landscape data, done


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


  @Then /^ExoCom now knows about these services:$/, (table, done) ->
    services = {}
    for row in table.hashes!
      services[row.NAME] =
        name: row.NAME
        host: row.HOST
        port: +row.PORT
    @verify-service-setup services, done


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
      eval livescript.compile "receiver-json = #{row.RECEIVERS}", bare: yes, header: no
      expected-routes[row.MESSAGE] =
        receivers: [receiver-json]
    @verify-routing-setup expected-routes, done


  @Then /^it runs at port (\d+)$/, (+port, done) ->
    @verify-runs-at-port port, done
