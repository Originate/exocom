require! {
  'chai' : {expect}
  'http'
  'livescript'
  'nitroglycerin' : N
  'portfinder'
  'request'
  '../support/text-tools' : {ascii}
  'wait' : {wait}
}


module.exports = ->

  @Given /^a "([^"]*)" instance running at port (\d+)$/, (name, port, done) ->
    @create-instance-at-port name, port, done
    (@ports or= {})[name] = port


  @Given /^an ExoComm instance$/, (done) ->
    portfinder.get-port N (@exocomm-port) ~>
      @create-exocomm-instance port: @exocomm-port, done


  @Given /^an ExoComm instance configured for the service landscape:$/, (table, done) ->
    portfinder.get-port N (@exocomm-port) ~>
      @create-exocomm-instance port: @exocomm-port, ~>
        data = for service in table.hashes!
          {
            name: service.NAME
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


  @Given /^the ([^ ]+) sends "([^"]*)" with id "([^"]*)"$/, (service, message, message-id, done) ->
    @last-sent-message = message
    @last-sent-request-id = message-id
    @service-sends-message {service, message, message-id}, done


  @When /^I( try to)? run ExoComm at port (\d+)$/, (!!expect-error, +port, done) ->
    @run-exocomm-at-port port, expect-error, done


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


  @When /^requesting the routing information$/, (done) ->
    @get-routing-information (@routing-information) ~>
      done!


  @When /^the (.+?) sends "([^"]*)"$/, (service, message, done) ->
    @last-sent-message = message
    @service-sends-message {service, message}, done


  @When /^the (.+)? sends "([^"]*)" in reply to "([^"]*)"$/, (service, reply-message, request-id, done) ->
    @last-sent-message = reply-message
    @service-sends-reply service, reply-message, request-id, done



  @Then /^ExoComm broadcasts this message to the (.*?)$/, (service-name, done) ->
    @verify-sent-calls {service-name, message: @last-sent-message, request-id: @last-sent-request-id}, done


  @Then /^ExoComm broadcasts this reply to the (.+?)$/, (service-name, done) ->
    @verify-sent-calls {service-name, message: @last-sent-message, response-to: '111'}, done



  @Then /^ExoComm now knows about these services:$/, (table, done) ->
    services = {}
    for row in table.hashes!
      services[row.NAME] =
        host: row.HOST
        port: +row.PORT
    @verify-service-setup services, done


  @Then /^ExoComm signals that this message is sent to the (.+)$/, (service-name, done) ->
    @verify-exocomm-broadcasted-message message: @last-sent-message, services: [service-name], done


  @Then /^ExoComm signals that this message was sent$/, (done) ->
    @verify-exocomm-broadcasted-message message: @last-sent-message, done


  @Then /^ExoComm signals that this reply is sent to the (.+)$/, (service-name, done) ->
    @verify-exocomm-broadcasted-message message: @last-sent-message, services: [service-name], response-to: '111', done


  @Then /^ExoComm signals that this reply was sent$/, (done) ->
    @verify-exocomm-broadcasted-reply @last-sent-message, done


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
