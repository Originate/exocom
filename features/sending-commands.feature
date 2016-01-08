Feature: Sending outgoing commands

  As an Exoservice developer
  I want my service to be able to send out Exosphere commands to the Exosphere environment
  So that it can send replies to incoming commands.

  Rules:
  - call "send" on your ExoRelay instance have it send out the given command


  Scenario: sending a valid command
    Given the Exosphere messaging infrastructure runs at port 4000
    And an ExoRelay instance
    When I send out a "yo" command:
      """
      exo-relay.send 'yo', done
      """
    Then it sends out the requests:
      """
      [
        {
          "body": {},
          "headers": {},
          "method": "POST",
          "url": "/send/yo",
        }
      ]
      """
