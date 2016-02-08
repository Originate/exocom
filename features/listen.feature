Feature: Listening

  As a developer building Exosphere applications
  I want to be able to add an Exosphere communication relay to any code base
  So that I can write Exosphere services without constraints on my code layout.

  Rules
  - call "listen" on an ExoRelay instance to take it online
  - the default port is 4000
  - you can provide a custom port as an argument to "listen"


  Background:
    Given ExoComm runs at port 4100
    And an ExoRelay instance


  Scenario: Setting up on the default port
    When I take it online at the default port
    Then it is online at port 4000


  Scenario: Setting up on a custom port
    When I take it online at port 4001
    Then it is online at port 4001


  # ERROR HANDLING

  Scenario: providing a non-number as the port
    When I try to take it online at port "zonk"
    Then ExoRelay emits an "error" event with the message "Non-numerical port provided to ExoRelay#listen"
