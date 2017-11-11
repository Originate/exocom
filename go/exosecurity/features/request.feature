Feature: Outgoing replies

  As an ExoService developer
  I want to be able to send outgoing responses to incoming messages
  So that I can build interactive services.

  Rules:
  - use the "helpers.Reply" method given as a parameter to your message handler
    to send replies for the message you are currently handling


  Background:
    Given the "complex" test fixture runs
    And receiving this message:
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
    And it sends the message:
      """
      {
        "name": "security request",
        "activityId": "444",
        "id": "666",
        "payload": {
          "name": "retrieve user session",
          "payload": "abc",
          "id": "123",
          "activityId": "456"
        }
      }
      """

  Scenario: determining a message is authorized
    When it receives the message:
      """
        {
          "name": "security response",
          "activityId": "444",
          "id": "777",
          "payload": {
            "name": "user session retrieved",
            "payload": {
              "isAdmin": "true"
            },
            "id": "random",
            "activityId": "456"
          }
        }
        """
    Then it sends the message:
      """
      {
        "name": "message authorized",
        "activityId": "444",
        "id": "666"
      }
      """

  Scenario: determining a user is not an admin
    When it receives the message:
      """
        {
          "name": "security response",
          "activityId": "444",
          "id": "777",
          "payload": {
            "name": "user session retrieved",
            "payload": {
              "isAdmin": "false"
            },
            "id": "random",
            "activityId": "456"
          }
        }
        """
    Then it sends the message:
      """
      {
        "name": "message unauthorized",
        "activityId": "444",
        "id": "666"
      }
      """

  Scenario: determining a user session is not found
    When it receives the message:
      """
        {
          "name": "security response",
          "activityId": "444",
          "id": "777",
          "payload": {
            "name": "user session not found",
            "id": "random",
            "activityId": "456"
          }
        }
        """
    Then it sends the message:
      """
      {
        "name": "message unauthorized",
        "activityId": "444",
        "id": "666"
      }
      """
