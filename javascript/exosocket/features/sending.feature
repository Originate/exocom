Feature: Sending outgoing messages

  Rules:
  - can send a given message
  - the name of the message is required
  - returns the sent message
    - contains generated fields: id and activityId (if not provided)


  Background:
    Given an ExoSocket instance with the role "test-service"
    And ExoSocket connects to ExoCom


  Scenario: sending a message without payload
    When sending the message "hello-world"
    Then ExoSocket makes the WebSocket request:
      """
      {
        "name": "hello-world",
        "sender": "test-service",
        "id": "{{outgoingMessageId}}",
        "activityId": "{{outgoingActivityId}}"
      }
      """


  Scenario: sending a message with whitespace
    When sending the message "hello world"
    Then ExoSocket makes the WebSocket request:
      """
      {
        "name": "hello world",
        "sender": "test-service",
        "id": "{{outgoingMessageId}}",
        "activityId": "{{outgoingActivityId}}"
      }
      """


  Scenario: sending a message with a populated Hash as payload
    When sending the message "hello" with the payload:
      """
      { "name": "world" }
      """
    Then ExoSocket makes the WebSocket request:
      """
      {
        "name": "hello",
        "payload": {
          "name": "world"
        },
        "sender": "test-service",
        "id": "{{outgoingMessageId}}",
        "activityId": "{{outgoingActivityId}}"
      }
      """

  Scenario: trying to send an empty message
    When trying to send an empty message
    Then ExoSocket errors with "Message must have a name"


  Scenario: sending a message with auth
    When sending the message "hello-world" with auth "1"
    Then ExoSocket makes the WebSocket request:
      """
      {
        "name": "hello-world",
        "sender": "test-service",
        "id": "{{outgoingMessageId}}",
        "activityId": "{{outgoingActivityId}}",
        "auth": "1"
      }
      """
