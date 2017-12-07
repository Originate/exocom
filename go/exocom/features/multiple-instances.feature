Feature: Multiple instances of services

  - send all messages for a given activity id to the same instance


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
      }
    ]
    """
    And two running "web" instances
    And a running "users" instance


  Scenario Outline: tolerates instances disconnecting
    Given the <OFFLINE_INSTANCE_ID> "web" instance disconnects
    When the "users" service sends "user created"
    Then ExoCom broadcasts the message "user created" to the <ONLINE_INSTANCE_ID> "web" instance

    Examples:
      | OFFLINE_INSTANCE_ID | ONLINE_INSTANCE_ID |
      | first               | second             |
      | second              | first              |


  Scenario Outline: sends to instances based on activity ids (service sends with the given activityId)
    Given the <INSTANCE_ID> "web" instance sends "create user" with activity "111"
    And ExoCom broadcasts the message "create user" to the "users" service
    When the "users" service sends "user created" for activity "111"
    Then ExoCom broadcasts the message "user created" to the <INSTANCE_ID> "web" instance

    Examples:
      | INSTANCE_ID |
      | first       |
      | second      |


  Scenario: sends to instances based on activity ids (service chosen to receive that given activity id)
    When the "users" service sends two "user created" messages for activity "111"
    Then one "web" instance receives two messages and the other receives none

  Scenario Outline: sends to instances based on activity ids, tolerating disconnects
    Given the <INSTANCE_ID> "web" instance sends "create user" with activity "111"
    And ExoCom broadcasts the message "create user" to the "users" service
    And the <INSTANCE_ID> "web" instance disconnects
    When the "users" service sends "user created" for activity "111"
    Then ExoCom broadcasts the message "user created" to the <OTHER_INSTANCE_ID> "web" instance

    Examples:
      | INSTANCE_ID | OTHER_INSTANCE_ID |
      | first       | second            |
      | second      | first             |
