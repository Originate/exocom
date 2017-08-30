Feature: Outgoing replies

  As an ExoService developer
  I want to be able to send outgoing responses to incoming messages
  So that I can build interactive services.

  Rules:
  - use the "helpers.Reply" method given as a parameter to your message handler
    to send replies for the message you are currently handling


  Background:
    Given I connect the "ping" test fixture


  Scenario: replying to a message
    When receiving a "ping" message with activityId "123"
    Then it sends a "pong" message as a reply to the message with activityId "123"


  Scenario: replying to a message with whitespace
    When receiving a "ping it" message with activityId "123"
    Then it sends a "pong it" message as a reply to the message with activityId "123"


  Scenario: replying to a message with auth
    When receiving a "ping" message with activityId "123" and auth "1"
    Then it sends a "pong" message as a reply to the message with activityId "123" and auth "1"


  Scenario: replying to a message with security
    When receiving a "ping" message with activityId "123" and isSecurity true
    Then it sends a "pong" message as a reply to the message with activityId "123" and isSecurity true
