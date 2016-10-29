Feature: Broadcasting messages

  As an ExoService
  I want to broadcast messages to other services
  So that I can communicate with them.

  - to broadcast a message, send a ZMQ request to the ZMQ port of the ExoCom Instance
  - ExoCom sends the message to all subscribed services


  Background:
    Given an ExoCom instance with routing information "[{name: web, receives: [created-user]}, {name: users, receives: [create-user]}]" configured for the service landscape:
      | NAME  | TYPE  | INTERNAL NAMESPACE | HOST      | PORT | SENDS        |
      | web   | web   | web                | localhost | 3001 | create-user  |
      | users | users | users              | localhost | 3002 | created-user |
    And a "web" instance running at port 3001
    And a "users" instance running at port 3002


  Scenario: broadcasting a message
    When the "web" service sends "create-user"
    Then ExoCom signals "web  --[ create-user ]->  users"
    And ExoCom broadcasts the message "create-user" to the "users" service


  Scenario: broadcasting a reply
    When the "web" service sends "create-user"
    And the "users" service sends "created-user" in reply to "111"
    Then ExoCom signals "users  --[ created-user ]->  web  (XX ms)"
    And ExoCom broadcasts the reply "created-user" to the "web" service
