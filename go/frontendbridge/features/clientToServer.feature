Feature:

  As an Exosphere developer
  I want to be able to send messages from a web client to my Exosphere application
  So that I can create a useful application

  Rules
  - If a message does not have an auth, a new one is generated for that user


  Background:
    Given a frontend bridge connected to Exocom


  Scenario: receiving a message without an auth
    When receiving this message from the client:
      """
      {
        "name": "hello",
        "id": "123"
      }
      """
    Then frontend bridge makes the websocket request:
      """
      {
        "name": "hello",
        "auth": "{{.sessionId}}",
        "sender": "websocket-test"
      }
      """


  Scenario: different clients get different auths when they connnect
    When receiving a message "hello" from 2 different clients
    Then those clients have different auths
