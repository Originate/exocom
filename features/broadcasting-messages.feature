Feature: Broadcasting messages

  As an ExoService
  I want to broadcast messages to other services
  So that I can communicate with them.

  Rules:
  - to broadcast a message, post to /send
  - ExoComm sends the message to all subscribed services


  Scenario: broadcasting a message
    Given a "web-server" instance running at port 3001
    And a "users-service" instance running at port 3002
    And an ExoComm instance configured for the service landscape:
      | NAME       | HOST      | PORT | SENDS       | RECEIVES    |
      | web-server | localhost | 3001 | create-user |             |
      | service 2  | localhost | 3002 |             | create-user |
    When the web-server sends "create-user"
    Then ExoComm signals that this message was sent
    And ExoComm broadcasts this message to the users-service
