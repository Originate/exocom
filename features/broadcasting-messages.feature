Feature: Broadcasting messages

  As an ExoService
  I want to broadcast messages to other services
  So that I can communicate with them.

  Rules:
  - to broadcast a message, post to /send
  - ExoCom sends the message to all subscribed services


  Background:
    Given a "web-server" instance running at port 3001
    And a "users-service" instance running at port 3002
    And an ExoCom instance configured for the service landscape:
      | NAME          | HOST      | PORT | SENDS        | RECEIVES     |
      | web-server    | localhost | 3001 | create-user  | created-user |
      | users-service | localhost | 3002 | created-user | create-user  |


  Scenario: broadcasting a message
    When the web-server sends "create-user"
    Then ExoCom signals that this message is sent from the web-server to the users-service
    And ExoCom broadcasts this message to the users-service


  Scenario: broadcasting a reply
    Given the web-server sends "create-user" with id "111"
    When the users-service sends "created-user" in reply to "111"
    Then ExoCom signals that this reply is sent from the users-service to the web-server
    And ExoCom broadcasts this reply to the web-server
