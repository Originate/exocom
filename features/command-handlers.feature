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
    Given an instance of the "log-to-console" service listening on port 4000


  Scenario: Sending a command
    When sending the request:
      """
      {
        "url": "http://localhost:4000/run/hello-world",
        "method": "POST"
      }
      """
    Then it returns a 200 response
    And its console output contains "Hello world!"


  Scenario: Sending a command with payload
    When sending the request:
      """
      {
        "url": "http://localhost:4000/run/hello-name",
        "method": "POST",
        "body": {
          "payload": {
            "name": "ExoRelay"
          }
        },
        "headers": {
          "content-type": "application/json"
        }
      }
      """
    Then it returns a 200 response
    And its console output contains "Hello ExoRelay!"


  Scenario: Sending a non-existing command
    When sending a POST request to "/run/zonk"
    Then it returns a 404 response
