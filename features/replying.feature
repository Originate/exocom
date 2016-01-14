Feature: Replying to incoming commands

  As an Exosphere developer
  I want my code to be able to reply to incoming Exosphere commands
  So that I can write responsive ExoServices.

  Rules:
  - if you send a command and want to handle any replies you receive for it,
    provide the reply handler as the second argument to "ExoRelay#send"
  - if you receive a command and want to send a reply for it,
    call the "reply" argument given to your command handler


  Background:
    Given ExoComm runs at port 4010
    And an ExoRelay instance called "exo-relay" listening at port 4000


  Scenario: handling replies to outgoing commands
    Given a hypothetical "@print" command
    And I send a command with a reply handler:
      """
      exo-relay.send 'users.create', name: 'Will Riker', (reply-payload) ~>
        @print "created user #{reply-payload.id}"
      """
    When a reply for the sent command arrives via this incoming request:
      """
      url: 'http://localhost:4000/run/users.created'
      method: 'POST'
      body:
        requestId: '123'
        responseTo: "<%= request_uuid %>"
        payload:
          id: 456
          name: 'Will Riker'
      headers:
        "content-type": "application/json"
      """
    Then the reply handler runs and in this example calls my "@print" method with "created user 456"


  Scenario: sending replies to incoming commands
    Given the "users.create" command has this handler:
      """
      exo-relay.register-handler 'users.create', (user-attributes, {reply}) ->
        # on this line we would create a user record with the given attributes in the database
        reply 'users.created', id: 456, name: user-attributes.name
      """
    When receiving this command via the incoming request:
      """
      url: 'http://localhost:4000/run/users.create'
      method: 'POST'
      body:
        payload:
          name: 'Will Riker'
        requestId: '123'
      """
    Then my command handler replies with a "users.created" command sent via this outgoing request:
      """
      url: 'http://localhost:4010/send/users.created'
      method: 'POST'
      body:
        payload:
          id: 456
          name: 'Will Riker'
        requestId: '<%= request_uuid %>'
        responseTo: '123'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """
