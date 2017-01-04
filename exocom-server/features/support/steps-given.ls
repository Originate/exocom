require! {
  'http'
  'nitroglycerin' : N
  'port-reservation'
  'wait' : {wait}
}


module.exports = ->

  @When /^a new "([^"]*)" service$/ (name, done) ->
    @create-mock-service-at-port {name, port: @exocom-port}, done


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
