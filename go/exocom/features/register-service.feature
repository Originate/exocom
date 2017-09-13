Feature: Manage new instances of services

  As an Exosphere operator
  I want that service instances can register themselves with ExoCom
  So that the system can add and remove instances at runtime and thereby scale with demand.

  Rules:
  - services send an "exocom.register" message when they come online
  - services are automatically de-registered when they go offline


  Scenario: a new service comes online
    Given an ExoCom instance configured with the routes:
      """
      [
        {
          "role": "users"
        }
      ]
      """
    When a "users" service connects and registers itself
    Then ExoCom should have the config:
      """
      {
        "clients": [
          {
            "role": "users",
            "instances": 1
          }
        ],
        "routes": {
          "users": {
            "sends": [],
            "receives": [],
            "messageTranslations": []
          }
        }
      }
      """


  Scenario: deregister a service once it goes offline
    Given an ExoCom instance configured with the routes:
      """
      [
        {
          "role": "users"
        },
        {
          "role": "tweets"
        }
      ]
      """
    And a running "users" instance
    And a running "tweets" instance
    When the "tweets" service goes offline
    Then ExoCom should have the config:
      """
      {
        "clients": [
          {
            "role": "users",
            "instances": 1
          }
        ],
        "routes": {
          "tweets": {
            "sends": [],
            "receives": [],
            "messageTranslations": []
          },
          "users": {
            "sends": [],
            "receives": [],
            "messageTranslations": []
          }
        }
      }
      """
