Feature: Handling incoming replies to sent message

  As an Exosphere developer
  I want my code to be able to handle replies to messages I send
  So that I can use the Exoservice landscape in my workflows.

  Rules:
  - provide the reply handler as the second argument to "ExoRelay#send"


  Background:
    Given ExoComm runs at port 4010
    And an ExoRelay instance called "exo-relay" listening at port 4000


  Scenario: handling replies to outgoing messages
    Given a hypothetical "print" message
    And I send a message with a reply handler:
      """
      exo-relay.send 'users.create', name: 'Will Riker', (reply-payload) ->
        print "created user #{reply-payload.id}"
      """
    When a reply for the sent message arrives via this incoming request:
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
    Then the reply handler runs and in this example calls my "print" method with "created user 456"


  Scenario: multi-level workflow
    When running this multi-level request:
      """
      exo-relay.send 'users.create', 'users.create payload', (users-created-payload) ~>
        exo-relay.send 'photos.store', 'photos.store payload', (photos-stored-payload) ->
          done!
      """
    Then ExoRelay sends the "users.create" message with payload "users.create payload"
    When receiving the "users.created" message with payload "users.created payload" as a reply to the "users.create" message
    Then ExoRelay sends the "photos.store" message with payload "photos.store payload"
    When receiving the "photos.stored" message with payload "photos.stored payload" as a reply to the "photos.store" message
    Then my handler calls the "done" method



  # ERROR HANDLING

  Scenario: providing a non-callable object as the reply handler
    When trying to send a message with a non-callable reply handler:
      """
      exo-relay.send 'users.create', {name: 'Will Riker'}, 'zonk'
      """
    Then ExoRelay emits an "error" event with the message "The reply handler given to ExoRelay#send must be a function"
