Feature: Listening

  As a developer testing services that use ExoRelay locally
  I want to be able to configure the port at which they listen to incoming commands
  So that I run more than one service locally while testing.

  Rules
  - call "listen" on an ExoRelay instance to take it online
  - the default port is 4000
  - you can provide a custom port as an argument to "listen"


  Background:
    Given an ExoRelay instance: "exoRelay = new ExoRelay()"


  Scenario: Setting up on the default port
    When I take it online at the default port: "exoRelay.listen(done);"
    Then it is online at port 4000


  Scenario: Setting up on a custom port
    When I take it online at port 4001: "exoRelay.listen(4001, done);"
    Then it is online at port 4001
