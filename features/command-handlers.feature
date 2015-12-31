Feature: Command handlers

  As a service developer
  I want to have an easy way to define code that handles incoming commands
  So that I can develop the services efficiently.


  Rules:
  - all handlers are defined in a file "src/server.ls"
  - this file exports a hash in which the key is the command name and the value the handler function
  - commands can be sent as a POST request to "/run/<command-name>"
    with the data payload as the request body


  Background:
    Given a running instance of the "log-to-console" service


  Scenario: Sending a valid command
    When sending a POST request to "/run/hello"
    Then it returns a 200 response
    And its console output contains "Hello there!"


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
    And its console output contains "Error: already called"
