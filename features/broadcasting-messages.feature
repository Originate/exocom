Feature: Broadcasting messages

  As an ExoService
  I want to broadcast messages to other services
  So that I can communicate with them.

  - to broadcast a message, post to /send
  - ExoCom sends the message to all subscribed services


  Background:
    Given a "web" instance running at port 3001
    And a "users" instance running at port 3002
    And an ExoCom instance configured for the service landscape:
      | NAME  | INTERNAL NAMESPACE | HOST      | PORT | SENDS        | RECEIVES     |
      | web   | web                | localhost | 3001 | create-user  | created-user |
      | users | users              | localhost | 3002 | created-user | create-user  |


  Scenario: broadcasting a message
    When the "web" service sends "create-user"
    Then ExoCom signals "web  --[ create-user ]->  users"
    And ExoCom broadcasts the message "create-user" to the "users" service


  Scenario: broadcasting a reply
    When the "users" service sends "created-user" in reply to "111"
    Then ExoCom signals "users  --[ created-user ]->  web"
    And ExoCom broadcasts the reply "created-user" to the "web" service
