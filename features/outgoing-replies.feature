Feature: Sending outgoing replies to incoming messages

  As an Exosphere developer
  I want my code to be able to send replies to incoming Exosphere messages
  So that I can write responsive ExoServices.

  Rules:
  - call the "reply" argument given to your message handler
    to send a reply to the message you are currently processing


  Background:
    Given ExoComm runs at port 4010
    And an ExoRelay instance called "exo-relay" listening at port 4000

  Scenario: sending a reply with JSON data
    Given the "users.create" message has this handler:
      """
      exo-relay.register-handler 'users.create', (user-attributes, {reply}) ->
        # on this line we would create a user record with the given attributes in the database
        reply 'users.created', id: 456, name: user-attributes.name
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/users.create'
      method: 'POST'
      body:
        payload:
          name: 'Will Riker'
        requestId: '123'
      """
    Then ExoRelay returns a 200 response
    And my message handler replies with a "users.created" message sent via this outgoing request:
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


  Scenario: sending a reply with string payload
    Given the "users.create" message has this handler:
      """
      exo-relay.register-handler 'ping', (_payload, {reply}) ->
        reply 'pong', 'from the test'
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/ping'
      method: 'POST'
      body:
        requestId: '123'
      """
    Then ExoRelay returns a 200 response
    And my message handler replies with a "users.created" message sent via this outgoing request:
      """
      url: 'http://localhost:4010/send/pong'
      method: 'POST'
      body:
        payload: 'from the test'
        requestId: '<%= request_uuid %>'
        responseTo: '123'
      headers:
        accept: 'application/json'
        'content-type': 'application/json'
      """



  # ERROR CHECKING
  # the "reply" method calls "ExoRelay#send", so it is tested there.
