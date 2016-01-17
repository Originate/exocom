Feature: Command handlers

  As a service developer
  I want to have an easy way to define code that handles incoming commands
  So that I can develop the services efficiently.


  Rules:
  - all handlers are defined in a file "src/server.ls"
  - this file exports a hash in which the key is the command name and the value the handler function
  - commands can be sent to the service as a POST request to "/run/<command-name>"
    with the data payload as the request body
  - data payload of commands goes in the request body, JSON encoded


  Background:
    Given ExoComm is available at port 4010
    And this instance of the "test" service:
      """
      exo-js run --port 4000 --exocomm-port=4010
      """


  Scenario: Sending a command
    When sending the request:
      """
      url: 'http://localhost:4000/run/hello-world'
      method: 'POST'
      body:
        requestId: '123'
      """
    Then it returns a 200 response
    And its console output contains "Hello world!"


  Scenario: Sending a command with payload
    When sending the request:
      """
      url: 'http://localhost:4000/run/hello-name'
      method: 'POST'
      body:
        payload:
          name: 'ExoRelay'
        requestId: '123'
      headers:
        'content-type': 'application/json'
      """
    Then it returns a 200 response
    And its console output contains "Hello ExoRelay!"


  Scenario: A command replies
    When sending the request:
      """
      url: 'http://localhost:4000/run/ping',
      method: 'POST'
      body:
        requestId: '123'
      """
    Then my service returns a 200 response
    And it sends the command "pong"


  Scenario: Sending a non-existing command
    When sending the request:
      """
      url: 'http://localhost:4000/run/zonk'
      method: 'POST'
      body:
        requestId: '123'
      """
    Then it returns a 404 response
