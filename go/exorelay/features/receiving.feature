Feature: Receiving messages

  As an Exosphere developer
  I want my code base to be able to respond to incoming messages
  So that I can create Exoservices.

  Rules:
  - Incoming messages are provided via a channel


  Background:
    Given an ExoRelay with the role "test-service"
    And ExoRelay connects to Exocom
    And I setup the "receiving-messages" test fixture


  Scenario: receiving a message without payload
    When receiving this message:
      """
      {
        "name": "hello-world",
        "id": "123"
      }
      """
    Then the fixture receives a message with the name "hello-world" and the payload nil


  Scenario: receiving a message with whitespace
    When receiving this message:
      """
      {
        "name": "hello world",
        "id": "123"
      }
      """
    Then the fixture receives a message with the name "hello world" and the payload nil


  Scenario: receiving a message with Hash payload
    When receiving this message:
      """
      {
        "name": "hello",
        "payload": {
          "name": "world"
        },
        "id": "123"
      }
      """
    Then the fixture receives a message with the name "hello" and the payload:
      """
      {
        "name": "world"
      }
      """

  Scenario: receiving a message with auth
    When receiving this message:
      """
      {
        "name": "hello-world",
        "id": "123",
        "auth": "1"
      }
      """
    Then the fixture receives a message with the name "hello-world" and auth "1"

  Scenario: receiveing a security message
    When receiving this message:
      """
      {
        "name": "hello-world",
        "id": "123",
        "isSecurity": true
      }
      """
    Then the fixture receives a message with the name "hello-world" and isSecurity true
