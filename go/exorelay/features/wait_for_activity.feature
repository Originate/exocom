Feature: wait for activity

  Rules:
  - use "Send" and "WaitForActivity" in order to send a message and wait for a reply


  Scenario: receiving a reply
    Given the "send_and_wait" test fixture runs with the role "test-service"
    When receiving a "pong" message with activityId "123"
    Then it sends a "pong received" message


  Scenario: timeout when waiting for a reply
    Given the "send_and_wait_timeout" test fixture runs with the role "test-service"
    When no message is received
    Then it sends a "pong not received" message
