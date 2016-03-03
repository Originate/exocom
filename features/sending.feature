Feature: Sending requests to services

  As a developer TDD-ing an ExoService
  I want to be able to send it messages in my tests
  So that I can trigger desired activities in my service and observe its behavior.


  Rules:
  - you must register services with the mock exocomm instance before you can send messages to them
  - call "exocomm.sendMessage service: <service-name>, name: <message name>, payload: <payload>"
    to send the given message to the given service


  Background:
    Given an ExoComMock instance


  Scenario: sending a message to a registered service
    Given a known "users" service listening at port 3010
    When sending a "users.create" message to the "users" service with the payload:
      """
      name: 'Jean-Luc Picard'
      """
    Then ExoComMock makes the request:
      | URL     | http://localhost:3010/run/users.create |
      | METHOD  | POST                                   |
      | PAYLOAD | name: 'Jean-Luc Picard'                |
    And ExoComMock lists the last send response code as 200


  Scenario: sending a message to an unknown service
    When trying to send a "users.create" message to the "users" service
    Then I get the error "unknown service: 'users'"
