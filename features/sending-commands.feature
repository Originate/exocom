Feature: Sending outgoing commands

  As an Exoservice developer
  I want my service to be able to send out Exosphere commands to the Exosphere environment
  So that it can send replies to incoming commands.

  Rules:
  - call "send" on your ExoRelay instance have it send out the given command


  Background:
    Given the Exosphere messaging infrastructure runs at port 4000
    And an ExoRelay instance

  Scenario: sending a stand-alone command
    When I send out a "hello-world" command:
      """
      exo-relay.send 'hello-world', done
      """
    Then it makes the requests:
      """
      [
        {
          url: "/send/hello-world",
          method: "POST",
          body: {},
          headers: {
            accept: "application/json",
            'content-type': "application/json"
          }
        }
      ]
      """


  Scenario: sending a stand-alone command with data payload
    When I send out a "hello" command with payload:
      """
      exo-relay.send command: 'hello', payload: { name: 'world' }, done
      """
    Then it makes the requests:
      """
      [
        {
          url: "/send/hello",
          method: "POST",
          body: {
            payload: {
              name: 'world'
            }
          },
          "headers": {
            accept: "application/json",
            "content-type": "application/json"
          }
        }
      ]
      """



  Scenario: sending a reply to another command
    When I send out a "yo" command with payload:
      """
      exo-relay.send command: 'hello', replying-to: 123, done
      """
    Then it makes the requests:
      """
      [
        {
          url: "/send/hello",
          method: "POST",
          body: {
            'replying-to': 123,
          },
          "headers": {
            accept: "application/json",
            "content-type": "application/json"
          }
        }
      ]
      """
