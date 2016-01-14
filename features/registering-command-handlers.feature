Feature: Registering command handlers

  As a deveoper writing Exosphere applications
  I want my application to respond to incoming commands
  So that I can write Exosphere services.

  Rules:
  - register command handlers by via "registerHandler"
  - the registered commands can be invoked by sending a POST request to "/run/<command-name>"
  - the response for that request indicates whether the command was understood, not that it was successful
  - commands are executed asynchronously, and can send other commands back to indicate responses


  Background:
    Given an ExoRelay instance called "exo-relay"


  Scenario: registering a command handler
    When I register a command handler:
      """
      exo-relay.register-handler 'command-1', ->
      """
    Then the instance has a handler for the command "command-1"


  Scenario: registering several handlers
    When I register the command handlers:
      """
      exo-relay.register-handlers command1: ->, command2: ->
      """
    Then the instance has handlers for the commands "command1" and "command2"


  Scenario: registering an already handled command
    Given my ExoRelay instance already has a handler for the command "hello"
    When I try to add another handler for that command
    Then ExoRelay throws an exception with the message "There is already a handler for command 'hello'"
