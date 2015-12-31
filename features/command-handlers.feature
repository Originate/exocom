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
    Given an instance of the "log-to-console" service


  Scenario: Sending a valid command
    When sending a POST request to "/run/hello_world"
    Then it returns a 200 response
    And its console output contains "Hello world!"


  Scenario: Sending a command with payload
    When sending a POST request to "/run/hello_name" with the payload
      """
      {"name": "Exosphere"}
      """
    And its console output contains "Hello Exosphere!"
    Then it returns a 200 response


  Scenario: Sending a non-existing command
    When sending a POST request to "/run/zonk"
    Then it returns a 404 response


  Scenario: the handler forgets to call "done"
    When sending a POST request to "/run/forget_done"
    Then it returns a 500 response
    And its console output contains "Problem with command 'forget_done': exceeded timeout"


  Scenario: the handler calls "done" twice
    When sending a POST request to "/run/double_done"
    Then it returns a 200 response
    And its console output contains "Problem with command 'double_done': already called"
