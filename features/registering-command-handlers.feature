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
    Given ExoComm runs at port 4100
    And an ExoRelay instance called "exo-relay"


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




  # ERROR CHECKING

  Scenario: registering an already handled command
    Given my ExoRelay instance already has a handler for the command "hello"
    When I try to add another handler for that command
    Then ExoRelay emits an "error" event with the message "There is already a handler for command 'hello'"


  Scenario: forgetting to provide the command
    When I try to register the command handler:
      """
      exo-relay.register-handler ->
      """
    Then ExoRelay emits an "error" event with the message "Request ids must be strings"


  Scenario: providing an empty command
    When I try to register the command handler:
      """
      exo-relay.register-handler '', ->
      """
    Then ExoRelay emits an "error" event with the message "No request id provided"


  Scenario: providing a non-string command
    When I try to register the command handler:
      """
      exo-relay.register-handler [], ->
      """
    Then ExoRelay emits an "error" event with the message "Request ids must be strings"


  Scenario: forgetting to provide the handler
    When I try to register the command handler:
      """
      exo-relay.register-handler 'command'
      """
    Then ExoRelay emits an "error" event with the message "No command handler provided"


  Scenario: providing a non-functional handler
    When I try to register the command handler:
      """
      exo-relay.register-handler 'command', 'zonk'
      """
    Then ExoRelay emits an "error" event with the message "Command handler must be a function"
