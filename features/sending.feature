Feature: Sending outgoing commands

  As an Exoservice developer
  I want my service to be able to send commands to other Exosphere services
  So that it can interact with the rest of the application.

  Rules:
  - call "send" on your ExoRelay instance have it send out the given command
  - provide the command to send as the first parameter
  - provide payload for the command as the second parameter
  - the payload can be either a string, an array, or a Hash


  Background:
    Given ExoComm runs at port 4000
    And an ExoRelay instance called "exo-relay" listening at port 4001


  Scenario: sending a command without payload
    When sending the "hello" command:
      """
      exo-relay.send 'hello-world'
      """
    Then ExoRelay makes the request:
      """
      url: 'http://localhost:4000/send/hello-world'
      method: 'POST'
      body:
        requestId: '<%= request_uuid %>'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """


  Scenario: sending a command with a Hash as payload
    When sending the "hello" command:
      """
      exo-relay.send 'hello', name: 'world'
      """
    Then ExoRelay makes the request:
      """
      url: 'http://localhost:4000/send/hello'
      method: 'POST'
      body:
        payload:
          name: 'world'
        requestId: '<%= request_uuid %>'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """


  Scenario: sending a command with a string as payload
    When sending the "hello" command:
      """
      exo-relay.send 'hello', 'world'
      """
    Then ExoRelay makes the request:
      """
      url: 'http://localhost:4000/send/hello'
      method: 'POST'
      body:
        payload: 'world'
        requestId: '<%= request_uuid %>'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """


  Scenario: sending a command with an array as payload
    When sending the "hello" command:
      """
      exo-relay.send 'sum', [1, 2, 3]
      """
    Then ExoRelay makes the request:
      """
      url: 'http://localhost:4000/send/sum'
      method: 'POST'
      body:
        payload: [1, 2, 3]
        requestId: '<%= request_uuid %>'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """


  Scenario: trying to send an empty command
    When trying to send an empty command:
      """
      exo-relay.send ''
      """
    Then ExoRelay throws an exception with the message "ExoRelay cannot send empty commands"


  Scenario: trying to send a non-string command
    When trying to send a non-string command:
      """
      exo-relay.send []
      """
    Then ExoRelay throws an exception with the message "ExoRelay can only send string commands"


  Scenario: forgetting to provide the payload when providing a reply handler
    When trying to send a non-serializable payload:
      """
      exo-relay.send 'test', ->
      """
    Then ExoRelay throws an exception with the message "ExoRelay cannot send functions as payload"
