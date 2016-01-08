Feature: Listening

  As a developer writing Exosphere applications
  I want to be able to talk to Exosphere from my Node.js application
  So that I can write Exosphere applications using Node.js.

  Rules
  - call "listen" with a port number on an ExoRelay instance to take it online at that port


  Scenario: Setting up on a free port
    Given an ExoRelay instance: "exoRelay = new ExoRelay()"
    When I take it online at port 4000: "exoRelay.listen(4000, done);"
    Then it is online at port 4000
