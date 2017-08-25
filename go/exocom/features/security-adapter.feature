Feature: Security Adapter

  Rules:
    - A service with the role security does not define sends/recieves. 
      They are automatically:
        "receives": ["authorize message", "security response"]
        "sends": ["message authorized", "message unauthorized", "security request"]
    - If any other service tries to list these messages in sends / receives, 
      exocom throws an error on startup
    - Exocom creates a new 'activityId' to be used for all security communication
      around a particular message
    - Exocom sends the message through if authorized
    - Exocom warns if a message is unauthorized and does not send the message
    - Exocom uses an 'isSecurity' flag to indiciate messages that are security
      requests / responses


  Background:
    Given an ExoCom instance configured with the routes:
      """
      [
        {
          "role": "web",
          "receives": ["user created"],
          "sends": ["create user"]
        },
        {
          "role": "users",
          "receives": ["create user"],
          "sends": ["user created"]
        },
        {
          "role": "security"
        },
        {
          "role": "sessions",
          "receives": ["create session", "get session"],
          "sends": ["session data"]
        }
      ]
      """
    And a running "web" instance
    And a running "users" instance
    And a running "security" instance
    And a running "sessions" instance
    And the "web" service sends:
      """
      {
        "name": "create user",
        "payload": {
          "name": "John Smith"
        },
        "id": "111",
        "auth": "222",
        "isSecurity": false,
        "activityId": "333"
      }
      """
    And ExoCom broadcasts the following message to the "security" service:
      """
      {
        "name": "authorize message",
        "payload": {
          "name": "create user",
          "payload": {
            "name": "John Smith"
          },
          "id": "111",
          "auth": "222",
          "isSecurity": false,
          "activityId": "333"
        },
        "id": "444",
        "activityId": "555"
      }
      """


  Scenario: message is authorized
    When the "security" service sends:
      """
      {
        "name": "message authorized",
        "id": "666",
        "activityId": "{{.outgoingActivityID}}"
      }
      """
    Then ExoCom broadcasts the following message to the "users" service:
      """
      {
        "name": "create user",
        "payload": {
          "name": "John Smith"
        },
        "id": "111",
        "auth": "222",
        "activityId": "333"
      }
      """


  Scenario: message is unauthorized
    When the "security" service sends:
      """
      {
        "name": "message unauthorized",
        "id": "666",
        "activityId": "{{.outgoingActivityID}}"
      }
      """
    Then ExoCom signals "Warning: Unauthorized message 'create user' from 'web' with activityId '333'"
