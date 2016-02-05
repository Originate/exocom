Feature: Listing receiving commands

  As a developer TDD-ing an ExoService
  I want to be able to verify commands it sends out
  So that I know it behaves correctly.

  Rules:
  - call "exocomm.receivedCommands" to get an array of received commands
  - call "exocomm.reset-calls" to reset the received calls list


  Scenario: no calls received yet
    Given an ExoCommMock instance
    Then it has received no commands


  Scenario: calls received
    Given an ExoCommMock instance listening at port 4100
    And somebody sends it a "hello" command with payload "world"
    And somebody sends it a "foo" command with payload "bar"
    Then it has received the commands
      | NAME  | PAYLOAD |
      | hello | world   |
      | foo   | bar     |


  Scenario: resetting the calls list after calls have been received
    Given an ExoCommMock instance listening at port 4100
    And somebody sends it a command
    When resetting the ExoCommMock instance
    Then it has received no commands
