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
    When receiving the "ping" command
    Then it acknowledges the received command
    And after a while it sends the "pong" command


  Scenario: receiving a command with payload
    When receiving the "greet" command with the payload:
      """
      name: 'world'
      """
    Then it acknowledges the received command
    And after a while it sends the "greeting" command with the textual payload:
      """
      Hello world
      """


  Scenario: receiving a non-existing command
    When receiving the unknown "zonk" command
    Then it signals an unknown command
