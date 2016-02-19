Feature: Sending from messages

  As an Exosphere developer
  I want to have access to a send method within my messages
  So that sending from messages is easy and my code concise.

  Rules:
  - call the "send" argument given to your message handler
    to send a message


  Background:
    Given ExoComm runs at port 4010
    And an ExoRelay instance called "exo-relay" running inside the "test" service at port 4000


  Scenario: sending a message from within a message handler
    Given the "users.login" message has this handler:
      """
      exo-relay.register-handler 'users.create', (_payload, {send}) ->
        send 'passwords.encrypt', 'secret'
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/users.create'
      method: 'POST'
      body:
        sender: 'test'
        requestId: '123'
      """
    Then ExoRelay returns a 200 response
    And my message handler sends out a "passwords.verify" message via this outgoing request:
      """
      url: 'http://localhost:4010/send/passwords.encrypt'
      method: 'POST'
      body:
        sender: 'test'
        payload: 'secret'
        requestId: '<%= request_uuid %>'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """

