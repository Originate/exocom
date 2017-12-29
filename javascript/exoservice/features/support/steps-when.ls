require! {
  'cucumber': {defineSupportCode}
  'livescript'
}


defineSupportCode ({Given, When, Then}) ->


  When /^receiving the( unknown)? "([^"]*)" message(?: with auth "([^"]*)")?$/, (expect-error, message-name, auth) ->
    @exocom
      ..reset!
      ..send @role, {name: message-name, expect-error, auth: auth}


  When /^receiving the( unknown)? "([^"]*)" message with the payload:$/, (expect-error, message-name, payload) ->
    eval livescript.compile "json-payload = {\n#{payload}\n}", bare: yes, header: no
    @exocom
      ..reset!
      ..send @role, {name: message-name, payload: json-payload, expect-error}


  When /^starting a service configured for ExoCom port (\d+)$/, (port, done) ->
    @role = 'test-service'
    @create-exoservice-instance {@role, exocom-port: port}, ~>
      @remove-register-service-message @exocom, done


  When /^starting the "([^"]*)" fixture$/, (@role, done) ->
    @create-exoservice-instance {@role, @exocom-port}, ~>
      @remove-register-service-message @exocom, done
