Feature: Receiving incoming commands

  As a deveoper writing Exosphere applications
  I want my application to respond to incoming commands
  So that I can write Exosphere services.

  Rules:
  - register command handlers by via "registerHandler"
  - the registered commands can be invoked by sending a POST request to "/run/<command-name>"
  - the response for that request indicates whether the command was understood, not that it was successful
  - commands are executed asynchronously, and can send other commands back to indicate responses


  Scenario: hello-world service
    Given an ExoRelay instance listening at port 4000
    And I add a command listener:
      """
      exo-relay.register-handler 'hello', ~>
        @ran = yes
      """
    When sending a POST request to "/run/hello"
    Then this command handler gets called
