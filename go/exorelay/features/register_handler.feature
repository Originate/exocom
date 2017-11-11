Feature: registering handlers

  Rules:
    - user "RegisterHandler" to define a function to handle messages with a given name
      - use "helpers.Reply" to send replies for the message you are currently handling
      - use "helpers.Send" and "helpers.WaitForActivity" in order to send


  Background:
    Given the "register_handler" test fixture runs with the role "test-service"


  Scenario: replying to a message
    When receiving a "ping" message with activityId "123"
    Then it sends a "pong" message as a reply to the message with activityId "123"


  Scenario: replying to a message with auth
    When receiving a "ping" message with activityId "123" and auth "1"
    Then it sends a "pong" message as a reply to the message with activityId "123" and auth "1"


  Scenario: replying to a message with security
    When receiving a "ping" message with activityId "123" and isSecurity true
    Then it sends a "pong" message as a reply to the message with activityId "123" and isSecurity true


  Scenario: sending a message before replying
    Given receiving a "complex ping" message with activityId "123"
    And it sends a "search" message
    When receiving a "result" message with the same activityId as the "search" message and the payload:
      """
      {
        "some": "data"
      }
      """
    Then it sends a "complex pong" message with activityId "123" and payload:
      """
      {
        "some": "data"
      }
      """
