Feature: Receiving commands

  As an Exosphere developer
  I want my code base to be able to respond to incoming commands
  So that I can create Exoservices.

  Rules:
  - you need to register handlers for commands that you want to receive
  - handlers are called with the request data


  Background:
    Given an ExoRelay instance listening at port 4000
    And a hypothetical "print" command


  Scenario: receiving a command without payload
    Given I register a handler for the "hello" command:
      """
      exo-relay.register-handler 'hello-world', ->
        print "Hello world!"
      """
    When receiving this command via the incoming request:
      """
      url: 'http://localhost:4000/run/hello-world',
      method: 'POST'
      body:
        requestId: '123'
      """
    Then ExoRelay runs the registered handler, in this example calling "print" with "Hello world!"


  Scenario: Receiving a command with string payload
    Given I register a handler for the "hello" command:
      """
      exo-relay.register-handler 'hello', (name) ->
        print "Hello #{name}!"
      """
    When receiving this command via the incoming request:
      """
      url: 'http://localhost:4000/run/hello',
      method: 'POST'
      body:
        payload: 'world'
        requestId: '123'
      """
    Then ExoRelay runs the registered handler, in this example calling "print" with "Hello world!"


  Scenario: receiving a command with Hash payload
    Given I register a handler for the "hello" command:
      """
      exo-relay.register-handler 'hello', ({name}) ->
        print "Hello #{name}!"
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
    Then ExoRelay runs the registered handler, in this example calling "print" with "Hello world!"


  Scenario: Receiving a command with array payload
    Given I register a handler for the "sum" command:
      """
      exo-relay.register-handler 'sum', (numbers) ->
        print numbers[0] + numbers[1]
      """
    When receiving this command via the incoming request:
      """
      url: 'http://localhost:4000/run/sum',
      method: 'POST'
      body:
        payload: [1, 2]
        requestId: '123'
      """
    Then ExoRelay runs the registered handler, in this example calling "print" with "3"



  # ERROR CHECKING

  Scenario: Trying to register a handler for an empty command name
    Given I try to register an empty command handler:
      """
      exo-relay.register-handler '', ->
      """
    Then ExoRelay throws an exception with the message "No request id provided"


  Scenario: Forgetting to provide the command name
    When I try to register a handler while forgetting to provide the command name:
      """
      exo-relay.register-handler ->
      """
    Then ExoRelay throws an exception with the message "Request ids must be strings"
