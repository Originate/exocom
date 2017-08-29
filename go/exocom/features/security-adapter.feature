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
    

  Scenario: message is authorized
    When ExoCom broadcasts the following message to the "security" service:
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
          "activityId": "333",
          "sender": "web"
        },
        "id": "444",
        "activityId": "555"
      }
      """
    Then the "security" service sends:
      """
      {
        "name": "message authorized",
        "id": "666",
        "activityId": "{{.outgoingActivityID}}"
      }
      """
    And ExoCom broadcasts the following message to the "users" service:
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
    When ExoCom broadcasts the following message to the "security" service:
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
          "activityId": "333",
          "sender": "web"
        },
        "id": "444",
        "activityId": "555"
      }
      """
    Then the "security" service sends:
      """
      {
        "name": "message unauthorized",
        "id": "666",
        "activityId": "{{.outgoingActivityID}}"
      }
      """
    And ExoCom signals "Warning: Unauthorized message 'create user' from 'web' with activityId '333'"


  Scenario: security request
    When the "security" service sends:
      """
      {
        "name": "security request",
        "payload": {
          "name": "get session",
          "payload": {
            "id": "2"
          },
          "id": "3",
          "activityId": "777"
        },
        "id": "666",
        "activityId": "555"
      }
      """
    Then ExoCom broadcasts the following message to the "sessions" service:
      """
      {
        "name": "get session",
        "payload": {
          "id": "2"
        },
        "id": "3",
        "activityId": "777",
        "isSecurity": true
      }
      """
    And the "sessions" service sends:
      """
      {
        "name": "session data",
        "payload": {
          "isAdmin": true
        },
        "id": "888",
        "activityId": "777",
        "isSecurity": true
      }
      """
    And ExoCom broadcasts the following message to the "security" service:
      """
      {
        "name": "security response",
        "payload": {
          "name": "session data",
          "payload": {
            "isAdmin": true
            },
          "id": "888",
          "activityId": "777",
          "isSecurity": true,
          "sender": "sessions"
        },
        "id": "999",
        "activityId": "555"
      }
      """

  Scenario: isSecurity flag cannot be used to bypass the security service
    When the "web" service sends:
      """
      {
        "name": "create user",
        "payload": {
          "name": "John Smith"
        },
        "id": "111",
        "auth": "222",
        "activityId": "333",
        "isSecurity": true
      }
      """
    Then ExoCom does not send any message to the "users" service
