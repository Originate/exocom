Feature: Sending requests to services

  As a developer TDD-ing an ExoService
  I want to be able to send it commands in my tests
  So that I can trigger desired activities in my service and observe its behavior.


  Rules:
  - you must register services with the mock exocomm instance before you can send commands to them
  - call "exocomm.send <service-name>, <command>, <payload>" to


  Scenario: sending a command to a registered service
    Given an ExoCommMock instance
    And a known "users" service listening at port 3010
    When sending a "users.create" command to the "users" service with the payload:
      """
      name: 'Jean-Luc Picard'
      """
    Then ExoCommMock makes the request:
      | URL     | http://localhost:3010/run/users.create |
      | METHOD  | POST                                   |
      | PAYLOAD | name: 'Jean-Luc Picard'                |


  Scenario: trying to send a command to an unknown service
    Given an ExoCommMock instance
    When trying to send a "users.create" command to the "users" service
    Then I get the error "unknown service: 'users'"
