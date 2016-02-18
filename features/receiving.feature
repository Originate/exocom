Feature: Listing receiving messages

  As a developer TDD-ing an ExoService
  I want to be able to verify messages it sends out
  So that I know it behaves correctly.

  Rules:
  - call "exocomm.receivedMessages" to get an array of received messages
  - call "exocomm.reset-calls" to reset the received calls list


  Scenario: no calls received yet
    Given an ExoCommMock instance
    Then it has received no messages


  Scenario: calls received
    Given an ExoCommMock instance listening at port 4100
    And somebody sends it a "hello" message with payload "world"
    And somebody sends it a "foo" message with payload "bar"
    Then it has received the messages
      | NAME  | PAYLOAD |
      | hello | world   |
      | foo   | bar     |


  Scenario: resetting the calls list after calls have been received
    Given an ExoCommMock instance listening at port 4100
    And somebody sends it a message
    When resetting the ExoCommMock instance
    Then it has received no messages
