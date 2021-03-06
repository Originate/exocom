Feature: Handling incoming replies to sent message

  As an Exosphere developer
  I want my code to be able to handle replies to messages I send
  So that I can use the Exoservice landscape in my workflows.

  Rules:
  - provide the reply handler as the second argument to "ExoRelay#send"


  Background:
    Given ExoCom runs at port 4100
    And an ExoRelay instance


  Scenario: handling replies to outgoing messages
    Given a "print" message
    And I send a message with a reply handler:
      """
      exo-relay.send 'users.create', name: 'Will Riker', (outcome, payload) ->
        if outcome is 'users.created'
          print "created user #{payload.id}"
      """
    When the reply arrives via this message:
      """
      name: 'users.created'
      payload:
        id: 456
        name: 'Will Riker'
      id: '123'
      activity-id: '<%= request_activity_id %>'
      """
    Then the reply handler runs and calls my "print" method with "created user 456"


  Scenario: handling replies to outgoing messages with spaces in message names
    Given a "print" message
    And I send a message with a reply handler:
      """
      exo-relay.send 'create users', name: 'Will Riker', (outcome, payload) ->
        if outcome is 'users created'
          print "created user #{payload.id}"
      """
    When the reply arrives via this message:
      """
      name: 'users created'
      payload:
        id: 456
        name: 'Will Riker'
      id: '123'
      activity-id: '<%= request_activity_id %>'
      """
    Then the reply handler runs and calls my "print" method with "created user 456"


  Scenario: multi-level workflow
    When running this multi-level request:
      """
      exo-relay.send 'users.create', 'users.create payload', ~>
        exo-relay.send 'photos.store', 'photos.store payload', ->
          done!
      """
    Then ExoRelay sends the "users.create" message with payload "users.create payload"
    When receiving the "users.created" message with payload "users.created payload" as a reply to the "users.create" message
    Then ExoRelay sends the "photos.store" message with payload "photos.store payload"
    When receiving the "photos.stored" message with payload "photos.stored payload" as a reply to the "photos.store" message
    Then my handler calls the "done" method



  # ERROR HANDLING

  Scenario: providing an invalid third argument
    When trying to send a message with a non-callable reply handler:
      """
      exo-relay.send 'users.create', {name: 'Will Riker'}, 'zonk'
      """
    Then ExoRelay emits an "error" event with the error "The third argument to ExoRelay#send must be an object (when supplying options) or a function (when supplying a reply handler)"

  Scenario: providing a non-callable object as the reply handler
    When trying to send a message with a non-callable reply handler:
      """
      exo-relay.send 'users.create', {name: 'Will Riker'}, {}, 'zonk'
      """
    Then ExoRelay emits an "error" event with the error "The fourth argument to ExoRelay#send must be a function"
