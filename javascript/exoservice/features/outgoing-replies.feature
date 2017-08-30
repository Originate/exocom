Feature: Outgoing replies

  As an ExoService developer
  I want to be able to send outgoing responses to incoming messages
  So that I can build interactive services.

  Rules:
  - use the "reply" method given as a parameter to your message handler
    to send replies for the message you are currently handling


  Background:
    Given an ExoCom instance
    And an instance of the "test-service" fixture


  Scenario: A message replies
    When receiving the "ping" message
    Then after a while it sends the "pong" message


  Scenario: A message replies with auth
    When receiving the "ping" message with auth "1"
    Then after a while it sends the "pong" message with auth "1"
