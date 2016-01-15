Feature: Handling incoming replies to sent commands

  As an Exosphere developer
  I want my code to be able to handle replies to commands I send
  So that I can use the Exoservice landscape in my workflows.

  Rules:
  - provide the reply handler as the second argument to "ExoRelay#send"


  Background:
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



  # ERROR HANDLING

  Scenario: providing a non-callable object as the reply handler
    When trying to send a command with a non-callable reply handler:
      """
      exo-relay.send 'users.create', {name: 'Will Riker'}, 'zonk'
      """
    Then ExoRelay throws an exception with the message "The reply handler given to ExoRelay#send must be a function"
