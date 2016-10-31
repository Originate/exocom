Feature: Broadcasting messages

  As an ExoService
  I want to broadcast messages to other services
  So that I can communicate with them.

  - to broadcast a message, send a ZMQ request to the ZMQ port of the ExoCom Instance
  - ExoCom sends the message to all subscribed services


  Background:
    Given an ExoCom instance with routing information "[{name: web, receives: [users.created]}, {name: users, receives: [users.create]}]"
    And a running "web" instance
    And a running "users" instance


  Scenario: broadcasting a message
    When the "web" service sends "users.create"
    Then ExoCom signals "web  --[ users.create ]->  users"
    And ExoCom broadcasts the message "users.create" to the "users" service


  Scenario: broadcasting a reply
    When the "web" service sends "users.create"
    And the "users" service sends "users.created" in reply to "111"
    Then ExoCom signals "users  --[ users.created ]->  web  (XX ms)"
    And ExoCom broadcasts the reply "users.created" to the "web" service
