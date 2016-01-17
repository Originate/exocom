Feature: Command handlers

  As a service developer
  I want to have an easy way to define code that handles incoming commands
  So that I can develop the services efficiently.


  Rules:
  - the handlers are defined in a file "server.ls"
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


  Scenario: receiving a command
    When receiving the "hello-world" command
    Then its console output contains "Hello world!"


  Scenario: receiving a command with payload
    When receiving the "hello-name" command with the payload:
      """
      {"name": "ExoRelay"}
      """
    Then its console output contains "Hello ExoRelay!"


  Scenario: receiving a non-existing command
    When receiving the unknown "zonk" command
    Then it returns a 404 response
