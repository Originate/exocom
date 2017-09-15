Feature: Outgoing replies

  As an ExoService developer
  I want to be able to send outgoing responses to incoming messages
  So that I can build interactive services.

  Rules:
  - use the "helpers.Reply" method given as a parameter to your message handler
    to send replies for the message you are currently handling


  Background:
    Given an ExoCom instance
    And an instance of the "simple" fixture


  Scenario: determining a message is authorized
    When receiving this message:
      """
      {
        "name": "authorize message",
        "payload": {
          "name": "create user",
          "id": "111",
          "activityId": "222",
          "auth": "abc"
        },
        "activityId": "444",
        "id": "555"
      }
      """
    Then it sends the message:
      """
      {
        "activityId": "444",
        "name": "message authorized",
        "payload": {},
        "sender": "simple",
        "id": "666"
      }
      """

  Scenario: determining a message is unauthorized
    When receiving this message:
     """
      {
        "name": "authorize message",
        "payload": {
          "name": "create user",
          "id": "111",
          "activityId": "222",
          "auth": "def"
        },
        "activityId": "444",
        "id": "555"
      }
      """
    Then it sends the message:
      """
      {
        "name": "message unauthorized",
        "activityId": "444",
        "id": "666",
        "payload": {},
        "sender": "simple"
      }
      """
