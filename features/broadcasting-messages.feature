Feature: Broadcasting messages

  As an ExoService
  I want to broadcast messages to other services
  So that I can communicate with them.

  Rules:
  - to broadcast a message, post to /send
  - ExoComm sends the message to all subscribed services


  Scenario: broadcasting a registered message to active listeners
    Given an ExoComm instance
    And a web server instance registered to send "create-user"
    And a users service instance registered to receive "create-user"
    When the web server sends "create-user"
    Then ExoComm broadcasts this message to the users service


  @todo
  Scenario: trying to broadcast an unregistered message
    Given an ExoComm instance
    And a web server instance registered to send "create user"
    And a users service instance registered to receive "create user"
    When the web server tries to send "unregistered command"
    Then ExoComm sends an "unregistered command" reply


  @todo
  Scenario: a registered receiver is offline
    Given an ExoComm instance
    And a users service instance registered to receive "create user"
    And that users service is no longer online
    When ExoComm tries to broadcast the "create user" command to the users service
    Then it removes it from its service list
