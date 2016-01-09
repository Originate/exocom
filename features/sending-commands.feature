Feature: Sending outgoing commands

  As an Exoservice developer
  I want my service to be able to send commands to other Exosphere services
  So that it can interact with the rest of the application.

  Rules:
  - call "send" on your ExoRelay instance have it send out the given command
  - provide payload through the named parameter "payload"
  - refer to the command you are replying in the named "replying-to" parameter


  Background:
    Given the Exosphere messaging infrastructure runs at port 4000
    And an ExoRelay instance


  Scenario: sending a stand-alone command
    When I send out a stand-alone command:
      """
      exo-relay.send command: 'hello-world'
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
    When I send out a stand-alone command with payload:
      """
      exo-relay.send command: 'hello', payload: { name: 'world' }
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
    When I send out a command in response to command '123':
      """
      exo-relay.send command: 'hello-world', replying-to: '123'
      """
    Then it makes the requests:
      """
      [
        {
          url: "/send/hello-world",
          method: "POST",
          body: {
            'replying-to': '123',
          },
          "headers": {
            accept: "application/json",
            "content-type": "application/json"
          }
        }
      ]
      """


  Scenario: sending a reply to another command with payload
    When I send out a command with payload in response to command '123':
      """
      exo-relay.send command: 'hello', payload: { name: 'world' }, replying-to: 123
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
            },
            'replying-to': 123,
          },
          "headers": {
            accept: "application/json",
            "content-type": "application/json"
          }
        }
      ]
      """
