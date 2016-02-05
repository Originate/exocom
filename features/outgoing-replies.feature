Feature: Outgoing replies

  As an ExoService developer
  I want to be able to send outgoing responses to incoming commands
  So that I can build interactive services.

  Rules:
  - use the "reply" method given as a parameter to your command handler
    to send replies for the command you are currently handling


  Background:
    Given ExoComm is available at port 4010
    And this instance of the "test" service:
      """
      exo-js run --port 4000 --exocomm-port=4010
      """


  Scenario: A command replies
    When receiving the "ping" command
    Then after a while it sends the "pong" command
