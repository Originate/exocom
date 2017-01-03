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
          "name": "users",
          "namespace": "foo"
        }
      ]
      """
    And a new "users" service
    Then ExoCom now knows about these services:
      | NAME  | INTERNAL NAMESPACE |
      | users | foo                |


  Scenario: deregister a service once it goes offline
    Given an ExoCom instance configured with the routes
    """
    [
      {
      "name": "users",
      "namespace": "foo"
      },
      {
      "name": "tweets",
      "namespace": "bar"
      }
    ]
    """
    And a running "users" instance
    And a running "tweets" instance
    When the "tweets" service goes offline
    Then ExoCom now knows about these services:
      | NAME  | INTERNAL NAMESPACE |
      | users | foo                |
