Feature: Sending outgoing commands

  As an Exoservice developer
  I want my service to be able to send commands to other Exosphere services
  So that it can interact with the rest of the application.

  Rules:
  - call "send" on your ExoRelay instance have it send out the given command
  - provide payload through the named parameter "payload"
  - refer to the command you are replying in the named "replying-to" parameter


  Background:
    Given ExoComm runs at port 4000
    And an ExoRelay instance called "exo-relay" listening at port 4001


  Scenario: sending a command
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
