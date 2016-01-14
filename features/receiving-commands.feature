Feature: Receiving commands

  As an Exosphere developer
  I want my code base to be able to respond to incoming commands
  So that I can create Exoservices.

  Rules:
  - you need to register handlers for commands that you want to receive
  - handlers are called with the request data


  Background:
    Given an ExoRelay instance listening at port 4000


  Scenario: receiving a command
    Given a hypothetical "@print" command
    And I register a handler for the "hello" command:
      """
      exo-relay.register-handler 'hello', (payload) ~>
        @print "Hello #{payload.name}!"
      """
    When receiving this command via the incoming request:
      """
      url: 'http://localhost:4000/run/hello',
      method: 'POST'
      body:
        payload:
          name: 'world'
        requestId: '123'
      """
    Then ExoRelay runs the registered handler, in this example calling "@print" with "Hello world!"
