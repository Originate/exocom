Feature: Sending outgoing commands

  As an Exoservice developer
  I want my service to be able to send commands to other Exosphere services
  So that it can interact with the rest of the application.

  Rules:
  - call "send" on your ExoRelay instance have it send out the given command
  - provide payload through the named parameter "payload"
  - refer to the command you are replying in the named "replying-to" parameter


  Background:
    Given ExoComm runs at port 4000
    And an ExoRelay instance listening at port 4001


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
          body: {
            "request-id": "<%= request_uuid %>"
          },
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
            },
            "request-id": "<%= request_uuid %>"
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
            "replying-to": '123',
            "request-id": "<%= request_uuid %>"
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
      exo-relay.send command: 'hello', payload: { name: 'world' }, replying-to: '123'
      """
    Then it makes the requests:
      """
      [
        {
          url: "/send/hello",
          method: "POST",
          body: {
            payload: {
              name: "world"
            },
            "request-id": "<%= request_uuid %>",
            "replying-to": "123"
          },
          "headers": {
            accept: "application/json",
            "content-type": "application/json"
          }
        }
      ]
      """


  Scenario: sending a command with a reply handler
    When I send out a command with a reply handler
      """
      exo-relay.send command: 'ping', ~>
        @reply-handled = yes
      """
    Then it makes the requests:
      """
      [
        {
          url: "/send/ping",
          method: "POST",
          body: {
            "request-id": "<%= request_uuid %>"
          },
          "headers": {
            accept: "application/json",
            "content-type": "application/json"
          }
        }
      ]
      """
    When the reply for this command arrives in the form of this incoming request:
      """
      {
        "url": "http://localhost:4001/run/pong",
        "method": "POST",
        "body": {
          "replying-to": "<%= request_uuid %>"
        },
        "headers": {
          "content-type": "application/json"
        }
      }
      """
    Then the reply handler is called, meaning:
      """
      @reply-handled is yes
      """
