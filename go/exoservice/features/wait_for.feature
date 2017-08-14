Feature: Outgoing messages

  As an Exoservice developer
  I want to be able to send other messages from my services
  So that my services can use other services.

  Rules:
  - use the "helpers.WaitFor" method given as a parameter to your message handler
    to wait for a reply for a sent message


  Background:
    Given I connect the "ping with dependency" test fixture


  Scenario:
    When receiving a "ping" message with activityId "123"
    Then it sends a "search" message
    And receiving a "result" message with the same activityId as the "search" message and the payload:
      """
      {
        "some": "data"
      }
      """
    Then it sends a "pong" message with activityId "123" and payload:
      """
      {
        "some": "data"
      }
      """
