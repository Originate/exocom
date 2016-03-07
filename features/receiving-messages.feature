Feature: Receiving messages

  As an Exosphere developer
  I want my code base to be able to respond to incoming messages
  So that I can create Exoservices.

  Rules:
  - you need to register handlers for messages that you want to receive
  - handlers are called with the request data


  Background:
    Given ExoCom runs at port 4100
    And an ExoRelay instance listening at port 4000
    And a hypothetical "print" message


  Scenario: receiving a message without payload
    Given I register a handler for the "hello" message:
      """
      exo-relay.register-handler 'hello-world', ->
        print "Hello world!"
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/hello-world',
      method: 'POST'
      body:
        id: '123'
      """
    Then ExoRelay returns a 200 response
    And it runs the registered handler, in this example calling "print" with "Hello world!"


  Scenario: Receiving a message with string payload
    Given I register a handler for the "hello" message:
      """
      exo-relay.register-handler 'hello', (name) ->
        print "Hello #{name}!"
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/hello',
      method: 'POST'
      body:
        payload: 'world'
        id: '123'
      """
    Then ExoRelay returns a 200 response
    And ExoRelay runs the registered handler, in this example calling "print" with "Hello world!"


  Scenario: receiving a message with Hash payload
    Given I register a handler for the "hello" message:
      """
      exo-relay.register-handler 'hello', ({name}) ->
        print "Hello #{name}!"
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/hello',
      method: 'POST'
      body:
        payload:
          name: 'world'
        id: '123'
      """
    Then ExoRelay returns a 200 response
    And ExoRelay runs the registered handler, in this example calling "print" with "Hello world!"


  Scenario: Receiving a message with array payload
    Given I register a handler for the "sum" message:
      """
      exo-relay.register-handler 'sum', (numbers) ->
        print numbers[0] + numbers[1]
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/sum',
      method: 'POST'
      body:
        payload: [1, 2]
        id: '123'
      """
    Then ExoRelay returns a 200 response
    And ExoRelay runs the registered handler, in this example calling "print" with "3"



  # ERROR CHECKING


  Scenario: a handler throws an exception
    Given I register a handler for the "sum" message:
      """
      exo-relay.register-handler 'boom', ->
        throw new Error 'boom!'
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/boom',
      method: 'POST'
      body:
        id: '123'
      """
    Then ExoRelay returns a 500 response



  Scenario: missing incoming message
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run',
      method: 'POST'
      body:
        id: '123'
      """
    Then ExoRelay returns a 404 response


  Scenario: the incoming message is not registered
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/zonk',
      method: 'POST'
      body:
        id: '123'
      """
    Then ExoRelay returns a 404 response with the text "unknown message: 'zonk'"


  Scenario: the incoming message has no id
    Given I register a handler for the "hello" message:
      """
      exo-relay.register-handler 'hello', ->
      """
    When receiving this message via the incoming request:
      """
      url: 'http://localhost:4000/run/hello',
      method: 'POST'
      """
    Then ExoRelay returns a 400 response with the text "missing message id"
