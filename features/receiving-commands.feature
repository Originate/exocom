Feature: Receiving commands

  As an Exosphere developer
  I want my code base to be able to respond to incoming commands
  So that I can create Exoservices.

  Rules:
  - you need to register handlers for commands that you want to receive
  - handlers are called with the request data


  Background:
    Given an ExoRelay instance listening at port 4000


  Scenario: basic incoming command
    Given I register a handler for the "hello" command
    When receiving the request:
      """
      {
        "url": "http://localhost:4000/run/hello",
        "method": "POST",
        "body": {},
        "headers": {
          "content-type": "application/json"
        }
      }
      """
    Then it calls the registered "hello" handler


  Scenario: incoming command with a payload
    Given I register a handler for the "hello" command
    When receiving the request:
      """
      {
        "url": "http://localhost:4000/run/hello",
        "method": "POST",
        "body": {},
        "headers": {
          "content-type": "application/json"
        }
      }
      """
    Then it calls the registered "hello" handler
