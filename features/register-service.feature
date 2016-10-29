Feature: Manage new instances of services

  As an Exosphere operator
  I want that service instances can register themselves with ExoCom
  So that the system can add and remove instances at runtime and thereby scale with demand.

  Rules:
  - services send an "exocom.register" message when they come online
  - services are automatically de-registered when they go offline


  Scenario: a new service comes online
    Given an ExoCom instance with routing information "[{name: users-service, receives: [users.create]}]"
    When a new service instance registers itself via the message:
      """
      * name: 'exocom.register-service'
        sender: 'users-service'
        payload:
          name: 'users-service'
          type: 'users'
          internal-namespace: 'foo'
          host: 'localhost'
          port: 4001
        id: '123'
      """
    Then ExoCom now knows about these services:
      | NAME          | TYPE  | INTERNAL NAMESPACE | HOST      | PORT |
      | users-service | users | foo                | localhost | 4001 |


  Scenario: deregister a service once it goes offline
    Given an ExoCom instance managing the service landscape:
      | NAME           | TYPE   | INTERNAL NAMESPACE | HOST      | PORT |
      | users-service  | users  | foo                | localhost | 4001 |
      | tweets-service | tweets | bar                | localhost | 4002 |
    When the "tweets-service" goes offline
    Then ExoCom now knows about these services:
      | NAME          | TYPE  | INTERNAL NAMESPACE | HOST      | PORT |
      | users-service | users | foo                | localhost | 4001 |
