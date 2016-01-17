Feature: Sending from commands

  As an Exosphere developer
  I want to have access to a send method within my commands
  So that sending from commands is easy and my code concise.

  Rules:
  - call the "send" argument given to your command handler
    to send a command


  Background:
    Given ExoComm runs at port 4010
    And an ExoRelay instance called "exo-relay" listening at port 4000

  Scenario: sending a command from within a command handler
    Given the "users.login" command has this handler:
      """
      exo-relay.register-handler 'users.create', (_payload, {send}) ->
        send 'passwords.encrypt', 'secret'
      """
    When receiving this command via the incoming request:
      """
      url: 'http://localhost:4000/run/users.create'
      method: 'POST'
      body:
        requestId: '123'
      """
    Then ExoRelay returns a 200 response
    And my command handler sends out a "passwords.verify" command via this outgoing request:
      """
      url: 'http://localhost:4010/send/passwords.encrypt'
      method: 'POST'
      body:
        payload: 'secret'
        requestId: '<%= request_uuid %>'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """

