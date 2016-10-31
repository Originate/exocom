Feature: Manage new instances of services

  As an Exosphere operator
  I want that service instances can register themselves with ExoCom
  So that the system can add and remove instances at runtime and thereby scale with demand.

  Rules:
  - services send an "exocom.register" message when they come online
  - services are automatically de-registered when they go offline


  Scenario: a new service comes online
    Given an ExoCom instance with routing information "[{name: users-service, receives: [users.create]}]"
    When a new "users" service with namespace "foo" comes online
    Then ExoCom now knows about these services:
      | NAME  | INTERNAL NAMESPACE |
      | users | foo                |


  Scenario: deregister a service once it goes offline
    Given an ExoCom instance managing the service landscape:
      | NAME   | INTERNAL NAMESPACE |
      | users  | foo                |
      | tweets | bar                |
    When the "tweets" service goes offline
    Then ExoCom now knows about these services:
      | NAME  | INTERNAL NAMESPACE |
      | users | foo                |
