Feature: Sending outgoing requests

  As an Exoservice developer
  I want to be able to send other commands from my services
  So that my services can use other services.

  Rules:
  - use the "send" method given to your command handlers as a parameter


  Background:
    Given ExoComm is available at port 4010
    And this instance of the "test" service:
      """
      exo-js run --port 4000 --exocomm-port=4010
      """


  Scenario: a service sends out a command
    When receiving the "sender" command
    Then after a while it sends the "greetings" command with the textual payload:
      """
      from the sender service
      """
